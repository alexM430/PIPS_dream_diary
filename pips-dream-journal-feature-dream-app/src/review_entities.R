# Suggests people, animals and objects mentioned in a dream text, using the
# word lists in data/lexicons plus built-in dictionaries. Suggestions are
# only counted once the user confirms them.

review_entity_dictionaries <- function(lexicon_dir) {
    read_dictionary <- function(name) {
        path <- file.path(lexicon_dir, paste0(name, ".csv"))
        if (!file.exists(path)) 
            return(tibble::tibble(category = character(), word = character()))
        x <- readr::read_csv(path, show_col_types = FALSE)
        if (!all(c("category", "word") %in% names(x)) || anyNA(x[c("category", "word")])) 
            stop("Invalid dictionary: ", name)
        dplyr::transmute(x, category = stringr::str_to_lower(category), word = stringr::str_to_lower(word))
    }
    from_groups <- function(groups) {
        dplyr::bind_rows(lapply(names(groups), function(category) {
            tibble::tibble(category = category, word = strsplit(groups[[category]], "|", fixed = TRUE)[[1]])
        }))
    }
    animals <- from_groups(c(dog = "dog|dogs", cat = "cat|cats", snake = "snake|snakes", bird = "bird|birds", 
        horse = "horse|horses", spider = "spider|spiders", wolf = "wolf|wolves", bear = "bear|bears", 
        fish = "fish", shark = "shark|sharks", lion = "lion|lions", animal = "animal|animals"))
    people <- from_groups(c(mother = "mother|mothers|mom|mum|mommy|mummy", father = "father|fathers|dad|daddy", 
        parent = "parent|parents", son = "son|sons", daughter = "daughter|daughters", child = "child|children|baby|babies|kid|kids", 
        sister = "sister|sisters", brother = "brother|brothers", friend = "friend|friends|best friend|best friends", 
        aunt = "aunt|aunts", uncle = "uncle|uncles", cousin = "cousin|cousins", neighbour = "neighbour|neighbours|neighbor|neighbors", 
        doctor = "doctor|doctors", nurse = "nurse|nurses", teacher = "teacher|teachers|professor|professors", 
        partner = "partner|partners|boyfriend|girlfriend|husband|wife|spouse|fianc\u00E9|fianc\u00E9e|fiance|fiancee", 
        ex_partner = "ex boyfriend|ex girlfriend|ex husband|ex wife|ex partner", colleague = "colleague|colleagues|coworker|coworkers|co worker|co workers", 
        man = "man|men", woman = "woman|women", boy = "boy|boys", girl = "girl|girls"))
    objects <- from_groups(c(phone = "phone|phones|telephone|telephones|smartphone|smartphones|mobile phone", 
        door = "door|doors", house = "house|houses", car = "car|cars", key = "key|keys", bag = "bag|bags|suitcase|suitcases|backpack|backpacks", 
        money = "money|cash|wallet|wallets", clock = "clock|clocks|watch|watches", mirror = "mirror|mirrors", 
        train = "train|trains", plane = "plane|planes|airplane|airplanes|aeroplane|aeroplanes", stairs = "stairs|staircase|staircases", 
        knife = "knife|knives", book = "book|books", bed = "bed|beds", window = "window|windows", elevator = "elevator|elevators|lift|lifts", 
        computer = "computer|computers|laptop|laptops", ladder = "ladder|ladders", bridge = "bridge|bridges", 
        ball = "ball|balls", gun = "gun|guns|pistol|pistols", boat = "boat|boats|ship|ships", bike = "bike|bikes|bicycle|bicycles", 
        umbrella = "umbrella|umbrellas", photo = "photo|photos|photograph|photographs", table = "table|tables", 
        chair = "chair|chairs", passport = "passport|passports", clothes = "clothes|clothing|shirt|shirts|dress|dresses|trousers|pants"))
    original_objects <- read_dictionary("objects")
    animal_categories <- unique(c(animals$category, "pet", "pets", "rabbit", "mouse", "rat", "insect"))
    animals <- dplyr::bind_rows(animals, dplyr::filter(original_objects, category %in% animal_categories), 
        read_dictionary("animals"))
    list(people = dplyr::distinct(dplyr::bind_rows(read_dictionary("people"), people)), animals = dplyr::distinct(animals), 
        objects = dplyr::distinct(dplyr::bind_rows(dplyr::filter(original_objects, !category %in% animal_categories), 
            objects)))
}

review_detect <- function(text, dictionary) {
    text <- stringr::str_to_lower(stringr::str_replace_all(dplyr::coalesce(text, ""), "[\u2019']", ""))
    clauses <- stringr::str_split(text, "[.!?;,:\\n]+")[[1]]
    tokens <- lapply(clauses, function(x) stringr::str_extract_all(x, "\\p{L}+")[[1]])
    hit <- vapply(dictionary$word, function(word) {
        phrase <- stringr::str_extract_all(stringr::str_to_lower(stringr::str_remove_all(word, "[\u2019']")), 
            "\\p{L}+")[[1]]
        k <- length(phrase)
        if (!k) 
            return(FALSE)
        any(vapply(tokens, function(t) {
            if (length(t) < k) return(FALSE)
            any(vapply(seq_len(length(t) - k + 1L), function(i) identical(t[i:(i + k - 1L)], phrase), 
                logical(1)))
        }, logical(1)))
    }, logical(1))
    review_normalise(dictionary$category[hit])
}

review_suggest_entities <- function(text, dictionaries, names) {
    people <- review_detect(text, dictionaries$people)
    if (nrow(names)) 
        people <- unique(c(people, review_detect(text, dplyr::transmute(names, category = person, word = alias))))
    list(people = people, animals = review_detect(text, dictionaries$animals), objects = review_detect(text, 
        dictionaries$objects))
}
