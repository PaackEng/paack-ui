module UI.I18n.Spanish exposing (..)

import UI.I18n.English as EmptyFallback
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


listView : ListView
listView =
    { search = "Buscar"
    , selectAll = EmptyFallback.listView.selectAll
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
    { ascending = EmptyFallback.tablesSorting.ascending
    , descending = EmptyFallback.tablesSorting.descending
    }


tables : Tables
tables =
    { details = tablesDetails
    , sorting = tablesSorting
    , selectRow = "Seleccionar esta fila."
    , selectAll = "Seleccionar todas las filas"
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
    , moreActions = "Mas acciones"
    }


contentPlaceholdersNothingToSeeHere : ContentPlaceholdersNothingToSeeHere
contentPlaceholdersNothingToSeeHere =
    { title = "Nada que ver aquí"
    , body = "Mira, trataste de mirar aquí. Esta bien, la vida puede ser difícil a veces cuando no encontramos lo que estamos buscando..."
    }


contentPlaceholders : ContentPlaceholders
contentPlaceholders =
    { nothingToSeeHere = contentPlaceholdersNothingToSeeHere
    }


dropdown : Dropdown
dropdown =
    { show = EmptyFallback.dropdown.show
    , collapse = EmptyFallback.dropdown.collapse
    }


calendar : Calendar
calendar =
    { jan = EmptyFallback.calendar.jan
    , feb = EmptyFallback.calendar.feb
    , mar = EmptyFallback.calendar.mar
    , apr = EmptyFallback.calendar.apr
    , may = EmptyFallback.calendar.may
    , jun = EmptyFallback.calendar.jun
    , jul = EmptyFallback.calendar.jul
    , aug = EmptyFallback.calendar.aug
    , sep = EmptyFallback.calendar.sep
    , oct = EmptyFallback.calendar.oct
    , nov = EmptyFallback.calendar.nov
    , dec = EmptyFallback.calendar.dec
    , mon = EmptyFallback.calendar.mon
    , tue = EmptyFallback.calendar.tue
    , wed = EmptyFallback.calendar.wed
    , thu = EmptyFallback.calendar.thu
    , fri = EmptyFallback.calendar.fri
    , sat = EmptyFallback.calendar.sat
    , sun = EmptyFallback.calendar.sun
    , prevMonth = EmptyFallback.calendar.prevMonth
    , nextMonth = EmptyFallback.calendar.nextMonth
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
    , contentPlaceholders = contentPlaceholders
    , dropdown = dropdown
    , calendar = calendar
    }
