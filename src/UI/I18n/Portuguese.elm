module UI.I18n.Portuguese exposing (..)

import UI.I18n.English as EmptyFallback
import UI.I18n.Types exposing (..)


filtersPeriod : FiltersPeriod
filtersPeriod =
    { after = "Após"
    , before = "Antes"
    , on = "Em"
    , description = "Seleccione o período de referência"
    }


filtersRange : FiltersRange
filtersRange =
    { from = EmptyFallback.filtersRange.from
    , to = EmptyFallback.filtersRange.to
    }


filtersSelect : FiltersSelect
filtersSelect =
    { description = EmptyFallback.filtersSelect.description
    }


filters : Filters
filters =
    { dateFormat = "DD/MM/YYYY"
    , close = "Fechar"
    , clear = "Limpar"
    , apply = "Aplique"
    , period = filtersPeriod
    , range = filtersRange
    , select = filtersSelect
    }


paginator : Paginator
paginator =
    { format = EmptyFallback.paginator.format
    , previous = "Anterior"
    , next = "Próxima"
    }


checkbox : Checkbox
checkbox =
    { toggle = EmptyFallback.checkbox.toggle
    }


listView : ListView
listView =
    { search = "Procurar"
    , selectAll = "Seleccionar todos"
    }


radio : Radio
radio =
    { select = EmptyFallback.radio.select
    }


tablesDetails : TablesDetails
tablesDetails =
    { show = EmptyFallback.tablesDetails.show
    , collapse = EmptyFallback.tablesDetails.collapse
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
    , selectRow = EmptyFallback.tables.selectRow
    , selectAll = EmptyFallback.tables.selectAll
    }


dateInput : DateInput
dateInput =
    { invalid = EmptyFallback.dateInput.invalid
    }


dialog : Dialog
dialog =
    { close = EmptyFallback.dialog.close
    }


sidebar : Sidebar
sidebar =
    { expand = EmptyFallback.sidebar.expand
    , collapse = EmptyFallback.sidebar.collapse
    , previous = EmptyFallback.sidebar.previous
    , moreActions = EmptyFallback.sidebar.moreActions
    }


contentPlaceholdersNothingToSeeHere : ContentPlaceholdersNothingToSeeHere
contentPlaceholdersNothingToSeeHere =
    { title = EmptyFallback.contentPlaceholdersNothingToSeeHere.title
    , body = EmptyFallback.contentPlaceholdersNothingToSeeHere.body
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
    { jan = "Janeiro"
    , feb = "Fevereiro"
    , mar = "Março"
    , apr = "Abril"
    , may = "Maio"
    , jun = "Junho"
    , jul = "Julho"
    , aug = "Agosto"
    , sep = "Setembro"
    , oct = "Outubro"
    , nov = "Novembro"
    , dec = "Dezembro"
    , mon = "Seg"
    , tue = "Ter"
    , wed = "Qua"
    , thu = "Qui"
    , fri = "Sex"
    , sat = "Sáb"
    , sun = "Dom"
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
