/// Flix supports stratified negation.

/// We declare several relations related to movies:
rel Movie(title: Str)
rel Actor(title: Str, name: Str)
rel Director(title: Str, name: Str)
rel PureMovie(title: Str)

/// We declare some facts about movies:
Movie("Interstellar").
Movie("The Hateful Eight").

/// We declare some facts about actors:
Actor("Interstellar", "Matthew McConaughey").
Actor("Interstellar", "Anne Hathaway").
Actor("The Hateful Eight", "Samuel L. Jackson").
Actor("The Hateful Eight", "Kurt Russel").
Actor("The Hateful Eight", "Quentin Tarantino").

/// We declare some facts about directors:
Director("Interstellar", "Christopher Nolan").
Director("The Hateful Eight", "Quentin Tarantino").

/// We can now compute all the movies in which the
/// director of the movie does not appear as an actor.
PureMovie(movie) :-
    Movie(movie),
    Director(movie, person),
    not Actor(movie, person).
