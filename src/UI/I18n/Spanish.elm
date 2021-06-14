module UI.I18n.Spanish exposing (..)

import UI.I18n.Types exposing (..)


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


dropdown : Dropdown
dropdown =
    { show = "Expandir"
    , collapse = "Colapsar"
    }


listView : ListView
listView =
    { search = "Buscar"
    , selectAll = "Select All"
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
    , selectRow = "Select this row."
    , selectAll = "Select all rows"
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
    , moreActions = "More actions"
    }


contentPlaceholdersNothingToSeeHere : ContentPlaceholdersNothingToSeeHere
contentPlaceholdersNothingToSeeHere =
    { title = "Nothing to see here"
    , body = "Look, you tried looking here. It’s ok, life can be difficult at times when we don’t find what we’re looking for..."
    }


contentPlaceholders : ContentPlaceholders
contentPlaceholders =
    { nothingToSeeHere = contentPlaceholdersNothingToSeeHere
    }


root : Root
root =
    { filters = filters
    , paginator = paginator
    , checkbox = checkbox
    , dropdown = dropdown
    , listView = listView
    , radio = radio
    , tables = tables
    , dateInput = dateInput
    , dialog = dialog
    , sidebar = sidebar
    , contentPlaceholders = contentPlaceholders
    }
