module UI.I18n.French exposing (..)

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
    { increase = "Trier de A à Z"
    , decrease = "Trier de Z à A"
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
    , listView = listView
    , radio = radio
    , tables = tables
    , dateInput = dateInput
    , dialog = dialog
    , sidebar = sidebar
    , contentPlaceholders = contentPlaceholders
    }
