namespace Bank {

    rel Customer(name: String, address: String)

    rel Account(accNumber: Int, accName: String, customerName: String)

    rel Branch(address: String)

}