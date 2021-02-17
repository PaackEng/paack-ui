module I18n.Spanish exposing (..)

import I18n.Types exposing (..)


filtersPeriod : FiltersPeriod
filtersPeriod =
    { after = "Después"
    , before = "Antes"
    , on = "En"
    , description = "Seleccionar período de referencia"
    }


filtersRange : FiltersRange
filtersRange =
    { from = \{ date } -> "Desde: " ++ date ++ ""
    , to = \{ date } -> "Hasta: " ++ date ++ ""
    }


filtersSelect : FiltersSelect
filtersSelect =
    { description = "Seleccionar opción para filtrar"
    }


filters : Filters
filters =
    { dateFormat = "DD/MM/YYYY"
    , close = "Cerrar"
    , clear = "Limpiar"
    , apply = "Aplicar"
    , period = filtersPeriod
    , range = filtersRange
    , select = filtersSelect
    }


paginator : Paginator
paginator =
    { format = \{ first, last, total } -> "" ++ first ++ " - " ++ last ++ " de " ++ total ++ ""
    , previous = "Anterior"
    , next = "Siguiente"
    }


checkbox : Checkbox
checkbox =
    { toggle = "Agrupar"
    }


listView : ListView
listView =
    { search = "Buscar"
    }


radio : Radio
radio =
    { select = "Seleccionar item"
    }


tablesDetails : TablesDetails
tablesDetails =
    { show = "Expandir"
    , collapse = "Colapsar"
    }


tablesSorting : TablesSorting
tablesSorting =
    { increase = "Ordenar A-Z"
    , decrease = "Ordenar Z-A"
    }


tables : Tables
tables =
    { details = tablesDetails
    , sorting = tablesSorting
    }


dateInput : DateInput
dateInput =
    { invalid = "Formato de fecha inválido"
    }


dialog : Dialog
dialog =
    { close = "Cerrar dialogo"
    }


sidebar : Sidebar
sidebar =
    { expand = "Expandir barra lateral"
    , collapse = "Minimizar barra lateral"
    , previous = "Atrás"
    }


root : Root
root =
    { filters = filters
    , paginator = paginator
    , checkbox = checkbox
    , listView = listView
    , radio = radio
    , tables = tables
    , dateInput = dateInput
    , dialog = dialog
    , sidebar = sidebar
    }
