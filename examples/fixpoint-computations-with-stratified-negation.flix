rel Movie(title: Str)
rel StarringIn(title: Str, name: Str)
rel DirectedBy(title: Str, name: Str)
rel DirectorNotInMovie(title: Str)

Movie("The Hateful Eight").
Movie("Interstellar").

StarringIn("The Hateful Eight", "Samuel L. Jackson").
StarringIn("The Hateful Eight", "Kurt Russel").
StarringIn("The Hateful Eight", "Quentin Tarantino").
StarringIn("Interstellar", "Matthew McConaughey").
StarringIn("Interstellar", "Anne Hathaway").

DirectedBy("The Hateful Eight", "Quentin Tarantino").
DirectedBy("Interstellar", "Christopher Nolan").

DirectorNotInMovie(title) :-
    Movie(title), DirectedBy(title, name), not StarringIn(title, name).

