///// Flix supports stratified negation.
//
///// We declare some facts about movies:
//Movie("Interstellar").
//Movie("The Hateful Eight").
//
///// We declare some facts about artists:
//Artist("Interstellar", "Matthew McConaughey").
//Artist("Interstellar", "Anne Hathaway").
//Artist("The Hateful Eight", "Samuel L. Jackson").
//Artist("The Hateful Eight", "Kurt Russel").
//Artist("The Hateful Eight", "Quentin Tarantino").
//
///// We declare some facts about directors:
//Director("Interstellar", "Christopher Nolan").
//Director("The Hateful Eight", "Quentin Tarantino").
//
///// We can now compute all the movies in which the
///// director of the movie does not appear as an artist.
//PureMovie(movie) :-
//    Movie(movie),
//    Director(movie, person),
//    not Artist(movie, person).
