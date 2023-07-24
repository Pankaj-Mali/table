defmodule TableWeb.Present do

  use TableWeb, :live_view

  alias TableWeb.Component.Table
  def mount(_params, _session, socket) do
    data = [
      %{name: "pankaj" ,
    sr_name: "mali",
    date: 98
  },
  %{name: "pakaj" ,
  sr_name: "mli",
  date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pakaj" ,
sr_name: "mli",
date: 98
},
%{name: "pankaj" ,
sr_name: "mali",
date: 90
},
%{name: "pankaj" ,
sr_name: "mali",
date: 94
},
%{name: "ram" ,
sr_name: "mali",
date: 98
}
    ]

    {:ok, assign(socket, data: data)}
  end

  def render(assigns) do

    ~H"""
    <div>
    <p class="text-center text-red-500">I Am presenting here </p>

    <Table.table data={@data} id={"pankaj"} >
    <:col :let={{_row, _col, val}} id={:name} name="name">
            <%= val %>
        </:col>
        <:col :let={{_row, _col, val}} id={:sr_name} name="sr_name">
            <%= val %>
        </:col>
        <:col :let={{_row, _col, val}} id={:date} name="any_num">
            <%= val %>
        </:col>
    </Table.table>
    </div>

    """

  end
end
