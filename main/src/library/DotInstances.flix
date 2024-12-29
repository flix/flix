instance DotGet_fst[(a, b)] {
    type Aef = {}
    type FieldType = a
    pub def get(tuple: (a, b)): a = let (x, _) = tuple; x
}

instance DotGet_len[Array[a, r]] {
    type Aef = r
    type FieldType = Int32
    pub def get(arr: Array[a, r]): Int32 \ r = arr->len
}

instance DotGet_snd[(a, b)] {
    type Aef = {}
    type FieldType = b
    pub def get(tuple: (a, b)): b = let (_, y) = tuple; y
}
