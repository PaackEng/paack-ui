module UI.I18n.French exposing (..)

import UI.I18n.English as EmptyFallback
import UI.I18n.Types exposing (..)


filtersPeriod : FiltersPeriod
filtersPeriod =
    { after = "Après"
    , before = "Avant"
    , on = "Sur"
    , description = "Sélectionner la période de référence"
    }


filtersRange : FiltersRange
filtersRange =
    { from = \{ date } -> "De:" ++ date ++ ""
    , to = \{ date } -> "A:" ++ date ++ ""
    }


filtersSelect : FiltersSelect
filtersSelect =
    { description = "Sélectionner l'option de filtrage"
    }


filters : Filters
filters =
    { dateFormat = "DD/MM/YYYY"
    , close = "Fermer"
    , clear = "Effacer"
    , apply = "Appliquer"
    , period = filtersPeriod
    , range = filtersRange
    , select = filtersSelect
    }


paginator : Paginator
paginator =
    { format = \{ first, last, total } -> "" ++ first ++ " - " ++ last ++ " sur " ++ total ++ "\n"
    , previous = "Précédent"
    , next = "Suivant"
    }


checkbox : Checkbox
checkbox =
    { toggle = "Basculer"
    }


listView : ListView
listView =
    { search = "Rechercher"
    , selectAll = EmptyFallback.listView.selectAll
    }


radio : Radio
radio =
    { select = "Sélectionner l'élément"
    }


tablesDetails : TablesDetails
tablesDetails =
    { show = "Elargir"
    , collapse = "Crash"
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
    , selectRow = "Sélectionner la ligne"
    , selectAll = "Sélectionner toutes les lignes"
    }


dateInput : DateInput
dateInput =
    { invalid = "Format de date invalide"
    }


dialog : Dialog
dialog =
    { close = "Fermer le dialogue"
    }


sidebar : Sidebar
sidebar =
    { expand = "Elargir la barre latérale"
    , collapse = "Réduire la barre latérale"
    , previous = "Retourner"
    , moreActions = "Autres actions"
    }


contentPlaceholdersNothingToSeeHere : ContentPlaceholdersNothingToSeeHere
contentPlaceholdersNothingToSeeHere =
    { title = "Rien a voir"
    , body = "Écoute, tu as essayé de regarder ici. C’est bon, la vie peut être difficile parfois quand on ne trouve pas ce qu’on cherche..."
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
