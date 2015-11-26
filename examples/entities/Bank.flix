namespace Bank {

    enum AccountType {
        case Checking,
        case Saving
    }

    enum CardType {
        case Debit,
        case Visa,
        case MasterCard
    }

    rel Customer(name: Str, address: Str)
    Customer("Victor K. Garcia", "4880 Lilac Lane")
    Customer("Melissa R. Morales", "1527 Boone Street")

    rel Account(accountNumber: Int, accountType: AccountType, customerName: Str, accountName: Str, amount: Int)
    Account(494863, AccountType.Checking, "Victor K. Garcia", "My Checking", 120)
    Account(494864, AccountType.Saving, "Victor K. Garcia", "My Savings", 5000)
    Account(326987, AccountType.Checking, "Melissa R. Morales", "Core Checking", 242)

    rel Card(cardNumber: Int, cardType: CardType, customerName: Str)
    Card(458156696, CardType.Debit, "Victor K. Garcia")
    Card(354878978, CardType.Visa, "Victor K. Garcia")
    Card(323214769, CardType.MasterCard, "Melissa R. Morales")

}