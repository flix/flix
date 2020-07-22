/// Polymorphically extends the record `r` with an `age` label.
/// Preserves (retains) all other labels polymorphically.
def withAge[a](r: {| a}, v: Int): {age: Int | a} =
    { +age = v | r }

/// Polymorphically restricts (removes) the `age` label from `r`.
/// Preserves (retains) all other labels polymorphically.
def withoutAge[a](r: {age: Int | a}): {| a} = {-age | r}

/// Construct several records and extend them with an age.
def main(): Int =
    let r1 = withAge({fstName = "Julius", lstName = "Caesar"}, 55);
    let r2 = withAge({monument = "Flavian Amphitheatre"}, 2019 - 80);
    let r3 = withAge({country = "United States"}, 2019 - 1776);
    r1.age + r2.age + r3.age

/// Constructs a record, extends it with an age, and restricts it.
def main2(): Unit =
    let r1 = {fstName = "Julius", lstName = "Caesar"};
    let r2 = withAge(r1, 55);
    let r3 = withoutAge(r2);
    ()
