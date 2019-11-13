/// Flix supports stratified negation for constraints.
/// The Flix compiler will statically ensure that use
/// of negation is safe and compute its stratification.

/// We declare several relations related to movies:
rel Movie(title: Str)
rel Artist(title: Str, name: Str)
rel Director(title: Str, name: Str)
rel PureMovie(title: Str)

/// We declare some facts about movies:
Movie("Interstellar").
Movie("The Hateful Eight").

/// We declare some facts about Artists:
Artist("Interstellar", "Matthew McConaughey").
Artist("Interstellar", "Anne Hathaway").
Artist("The Hateful Eight", "Samuel L. Jackson").
Artist("The Hateful Eight", "Kurt Russel").
Artist("The Hateful Eight", "Quentin Tarantino").

/// We declare some facts about directors:
Director("Interstellar", "Christopher Nolan").
Director("The Hateful Eight", "Quentin Tarantino").

/// We can now compute all the movies in which the
/// director of the movie does not appear as an Artist.
PureMovie(movie) :-
    Movie(movie),
    Director(movie, person),
    not Artist(movie, person).
