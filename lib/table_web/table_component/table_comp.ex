defmodule TableWeb.Component.Table do
  @moduledoc ~S"""
  Render a table with sorting, pagination.
  ## Example

  ```
  defmodule Component do
    use Phoenix.Component
    import TableWeb.Component.Table

    def render(assigns) do
      ~H\"\"\"
      <div>
        <.table :let={row} offset={0} limit={10} data={@rows} columns={columns()}>
          <!-- rendering for :id only -->
          <:col :let={col} key={:id}>
            <%= live_redirect(col, to: Routes.meter_object_path(Endpoint, :index, col) ) %>
          </:col>
        </.table>
      </div>
      \"\"\"
    end

    defp columns() do
      [
        %{name: "ID", id: :id},
        %{name: "Name", id: :name},
      ]
    end
  end
  ```
  """
  use TableWeb, :live_component

  require Logger

  @doc """
  Wrap the item
  """
  def table(%{id: _id} = assigns) do
    ~H"""
    <div>
      <.live_component module={__MODULE__} id={@id} {Map.drop(assigns, [:id])} />
    </div>
    """
  end

  def table(_assigns) do
    raise ArgumentError, "<#{__MODULE__}.table /> requires an :id attribute"
  end

  @impl true
  def render(%{rows: []} = assigns) do
    if [] != assigns.empty do
      ~H"""
      <div>
        <%= render_slot(@empty) %>
      </div>
      """
    else
      ~H"""
      <div class="text-center mt-5 pt-5 w-1/2 m-auto">
        <h4>Empty dataset</h4>
      </div>
      """
    end
  end

  def render(%{data: _data, rows: [_ | _], myself: _} = assigns) do
    assigns =
      assigns
      |> assign_new(:body, fn -> [] end)
      |> assign_new(:class, fn -> nil end)
      |> assign_new(:enumerate, fn -> false end)
      |> assign_new(:tr, fn -> fn _ -> [] end end)

    ~H"""
    <div>
      <div class="w-full overflow-x-scroll">
        <%= for {flash_key, flash_value} <- @flash do %>
          <div
            class={"rounded-none alert alert-#{flash_key}"}
            phx-click="lv:clear-flash"
            phx-value-key={flash_key}
            role="alert"
          >
            <div class="container mx-auto alert-inner">
              <%= case flash_value do
                %{icon: icon, text: text} ->
                  [
                    Heroicons.icon(icon,
                      type: flash_value[:type] || "solid",
                      class: "h-5 w-5 inline-block ml-1"
                    ),
                    text
                  ]

                %{text: text} ->
                  text

                "" <> text ->
                  text
              end %>
            </div>
          </div>
        <% end %>

        <table class={"table table-compact #{@class}"}>
          <thead>
            <tr>
              <%= if @enumerate do %>
                <th>
                  #
                </th>
              <% end %>
              <%= for col <- @columns do %>
                <th>
                  <%= if Map.get(col, :sortable, true) do %>
                    <a
                      class="cursor-pointer"
                      phx-click="table-sort"
                      phx-value-sort-by={col.id}
                      phx-value-sort-order={order(@sort_by == col.id, @sort_order)}
                      phx-target={@myself}
                    >
                      <%= col[:name] || col.id %> <%= if @sort_by == col.id, do: chevron(@sort_order) %>
                    </a>
                  <% else %>
                    <%= col[:name] || col.id %>
                  <% end %>
                </th>
              <% end %>
            </tr>
          </thead>
          <%= # Allow setting :body slot to do custom rendering, like multiple tbody tags,
          # for custom requirements.

          if [] != @body do %>
            <%= render_slot(@body, {@rows, @columns}) %>
          <% else %>
            <tbody>
              <%= for {row, idx} <- Enum.with_index(@rows) do %>
                <.row tr={@tr} enumerate={@enumerate} columns={@columns} idx={idx} row={row} />
              <% end %>
            </tbody>
          <% end %>
        </table>
      </div>
      <%= if @page_count > 1 do %>
        <div class="mt-5 px-5">
          <div class="text-right">
            <%= for page <- @pagination do %>
              <a
                class={"btn ml-1 #{if @page == page, do: "btn-info"}"}
                phx-click="table-set-page"
                phx-value-page={page}
                phx-target={@myself}
              >
                <%= page + 1 %>
              </a>
            <% end %>
          </div>
        </div>

        <div class="text-right">
          <span class="text-sm pr-5 mr-5 text-slate-500">
            Showing <%= @limit %>/<%= @total %> rows in page <%= @page + 1 %> of <%= @page_count %>
          </span>
        </div>
      <% end %>
    </div>
    """
  end

  defp row(assigns) do
    assigns = assign(assigns, :attrs, if(assigns.tr, do: assigns.tr.(assigns.row), else: []))

    ~H"""
    <tr {@attrs}>
      <%= if @enumerate do %>
        <td>
          <%= @idx + 1 %>
        </td>
      <% end %>
      <%= for col <- @columns, true != col[:hide] do %>
        <.cell row={@row} col={col} />
      <% end %>
    </tr>
    """
  end

  @doc """
  Render a table cell
  """
  def cell(%{row: row, col: col} = assigns) do
    passthrough = Map.take(assigns[:col], [:class, :headers, :rowspan, :colspan, :style, :title])

    assigns =
      assign_new(assigns, :value, fn -> get_value(row, col) end)
      |> assign_new(:passthrough, fn -> passthrough end)

    ~H"""
    <td {@passthrough}>
      <%= if @col[:__slot__], do: render_slot(@col, {@row, @col, @value}), else: @value %>
    </td>
    """
  end

  @impl true
  def mount(socket) do
    socket =
      assign(socket,
        rows: [],
        columns: [],
        sort_order: :desc,
        sort_by: nil,
        offset: 0,
        limit: 10
      )

    {:ok, assign(socket, columns: prepare_column_slots(socket.assigns))}
  end

  @impl true
  def update(assigns, socket) do
    defaults = %{
      class: "",
      limit: 10,
      offset: 0,
      total: nil,
      sort_order: :desc,
      local: true,
      sort_by: :id,
      data: [],
      empty: [],
      body: [],
      on_sort: nil,
      on_pagination: nil,
      enumerate: false,
      tr: fn _ -> [] end
    }

    keep = Map.keys(defaults)

    # Get the latest columns based on the slots.
    props =
      defaults
      |> Map.merge(socket.assigns)
      |> Map.merge(assigns)

    columns = prepare_column_slots(props)

    {{page, page_count}, pages} = pagination(props)

    rows =
      if false == assigns[:local] do
        assigns[:data]
      else
        []
      end

    new_assigns =
      defaults
      |> Map.merge(Map.take(socket.assigns, keep))
      |> Map.merge(Map.take(assigns, keep))
      |> Map.merge(%{
        columns: columns,
        rows: rows,
        page: page,
        total: assigns[:total] || length(assigns[:data]),
        offset: page * props.limit,
        page_count: page_count,
        pagination: pages
      })

    {:ok, safe_filter(assign(socket, new_assigns))}
  end

  @impl true
  def handle_event(
        "table-sort",
        %{"sort-by" => k, "sort-order" => order},
        %{assigns: assigns} = socket
      ) do
    columns = socket.assigns[:columns]
    default_sort_column = hd(columns)
    sort_column = Enum.find(columns, default_sort_column.id, &("#{&1.id}" == k))

    sort_order =
      case order do
        "asc" -> :asc
        "desc" -> :desc
      end

    next = %{
      sort_order: sort_order,
      sort_by: sort_column.id
    }

    case assigns.on_sort do
      nil ->
        {:noreply, safe_filter(assign(socket, next))}

      on_sort ->
        on_sort.(sort_column.id, sort_order)
        {:noreply, assign(socket, next)}
    end
  end

  def handle_event("table-set-page", %{"page" => page}, %{assigns: assigns} = socket) do
    {page, ""} = Integer.parse(page)

    {{page, page_count}, pages} = pagination(Map.put(assigns, :offset, page * assigns.limit))

    next = %{
      page: page,
      page_count: page_count,
      pagination: pages,
      offset: page * socket.assigns.limit,
      limit: socket.assigns.limit
    }

    case assigns.on_pagination do
      nil ->
        {:noreply, safe_filter(assign(socket, next))}

      on_pagination ->
        on_pagination.(next)
        {:noreply, assign(socket, next)}
    end
  end

  # In case `local` is disabled means no sorting and filtering is done by caller
  defp safe_filter(%{assigns: %{local: false}} = socket),
    do: assign(socket, rows: socket.assigns.data)

  defp safe_filter(%{assigns: assigns} = socket) do
    try do
      # Sortable columns may trigger error on malformed input in which we must warn
      assign(socket, :rows, get_filtered(socket.assigns))
    catch
      kind, err ->
        Logger.warning(Exception.format(kind, err, __STACKTRACE__))

        fallback =
          assigns.data
          |> skip(assigns)
          |> limit(assigns)

        socket =
          socket
          |> assign(:rows, fallback)
          |> put_flash(:warning, %{
            icon: "exclamation-circle",
            type: "outline",
            text:
              "Failed to render table correctly due to corrupt data, sorting, pagination may not work correctly."
          })

        socket
    end
  end

  # Get a up chevron depending on the sort order
  defp chevron(:desc),
    do: Heroicons.icon("chevron-down", type: "mini", class: "inline-block h-4 w-4")

  defp chevron(:asc),
    do: Heroicons.icon("chevron-up", type: "mini", class: "inline-block h-4 w-4")

  # Build a list of the available pages to show in pagination.
  # The result is a list of page numbers which can be used to
  # build a a series of pagination buttons
  defp pagination(%{limit: limit, total: total}) when 0 == limit or total == 0 do
    {{0, 1}, [0]}
  end

  defp pagination(%{offset: offset, limit: limit, data: data, total: total}) do
    show_next_n_pages = 2

    total = total || length(data)

    page = floor(offset / limit)
    page_count = ceil(total / limit)

    start = page - show_next_n_pages
    stop = page + show_next_n_pages

    # Get the out of bound value
    subtract_from_left = max(0, stop - page_count + 1)
    add_to_right = -1 * min(0, start - 1)

    # Shift by the out of bounds value from opposite side
    pagination_start = max(0, start - subtract_from_left)
    pagination_end = min(page_count - 1, stop + add_to_right)

    pages_available = Enum.to_list(pagination_start..pagination_end)
    pages = [0] ++ pages_available ++ [page_count - 1]

    {{page, page_count}, Enum.uniq(pages)}
  end

  # Prepare the data by filtering, sorting and slicing it.
  defp get_filtered(assigns) do
    # Keep default sorting and pagination here. You can use sort and slice
    # externally and set offset/limit, sort_by/sort_order accordingly if
    # you're using sorting/slicing from database
    assigns.data
    |> sort_by(assigns)
    |> skip(assigns)
    |> limit(assigns)
  end

  defp safe_sorter({order, module}) do
    case order do
      :desc ->
        fn
          nil, _b ->
            false

          _a, nil ->
            true

          a, b ->
            :lt != module.compare(a, b)
        end

      :asc ->
        fn
          nil, _b ->
            true

          _a, nil ->
            false

          a, b ->
            :gt != module.compare(a, b)
        end
    end
  end

  # Given a dataset and the current assigns, sort the dataset
  # by the selected key
  defp sort_by([fst | _] = data, %{sort_by: sort, columns: [head | _] = cols} = assigns)
       when nil != sort do
    sort_column = Enum.find(cols, head, &(&1.id == sort))

    sort_order =
      case assigns[:sort_order] do
        sorter when sorter in [:desc, :asc] -> sorter
        _ -> :desc
      end

    # Wrap this to make sorting DateTimes sortable. This expects that
    # the column contains homogeneous data.
    # unwrap it from `:value` otherwise the top level struct is used.
    sort_by_struct =
      case get_sort_value(fst, sort_column) do
        %{value: %s{}} -> s
        %s{} -> s
        _ -> nil
      end

    # DateTime sorter does not validate
    sorter =
      if function_exported?(sort_by_struct, :compare, 2) do
        safe_sorter({sort_order, sort_by_struct})
      else
        sort_order
      end

    sort_by = get_sort_by_column(sort_column)

    Enum.sort_by(data, sort_by, sorter)
  end

  defp sort_by(data, _), do: data

  defp get_sort_by_column(%{sort_by: sort_by} = sort_column) when is_function(sort_by, 1) do
    fn v ->
      sort_by.(get_sort_value(v, sort_column))
    end
  end

  defp get_sort_by_column(%{sort_by: sort_by} = sort_column) when is_function(sort_by, 2) do
    fn v ->
      sort_by.(get_sort_value(v, sort_column), v)
    end
  end

  defp get_sort_by_column(sort_column) do
    &get_sort_value(&1, sort_column)
  end

  defp skip(data, %{offset: e}) when nil != e, do: Enum.drop(data, e)
  defp skip(data, _), do: data

  defp limit(data, %{limit: e}) when nil != e, do: Enum.take(data, e)
  # defp limit(data, _), do: data

  defp order(_, :asc), do: :desc
  defp order(_, :desc), do: :asc

  defp get_value(row, %{accessor: accessor}), do: get_in(row, accessor)
  defp get_value(row, %{id: id}) when is_struct(row), do: get_in(row, [Access.key(id)])
  defp get_value(row, %{id: id}), do: get_in(row, [id])

  # Sort may required a different unpacking to avoid sorting by value (ie for DateTime sort)
  defp get_sort_value(row, %{sort_accessor: accessor}), do: get_in(row, accessor)
  defp get_sort_value(row, col), do: get_value(row, col)

  # Columns may defined by the :column property, by <:col/> slot or both.
  # If <:col /> slots are used the columns must be built for later lookup
  # in sort_by/2.
  defp prepare_column_slots(assigns) do
    slots = assigns[:col] || []
    columns = assigns[:columns] || []
    # If specifying columns with `columns={}` assign the default rendered
    # if it exists.
    columns =
      case Enum.find(slots, &(not Map.has_key?(&1, :id))) do
        nil ->
          columns

        defslot ->
          Enum.map(columns || [], &Map.merge(defslot, &1))
      end

    # Merge slots with `columns` so specialized rendering can be done
    # for any column.
    Enum.reduce(slots, columns, fn
      %{id: id} = slot, acc ->
        case Enum.find_index(acc, &(&1.id == id)) do
          # case List.update_at(acc, &(&1.id == id)) do
          nil ->
            acc ++ [slot]

          idx ->
            List.update_at(acc, idx, &Map.merge(&1, slot))
        end

      %{}, acc ->
        # If the slot has no ID it is used as the default render slot
        # for all cells
        acc
    end)
  end
end
