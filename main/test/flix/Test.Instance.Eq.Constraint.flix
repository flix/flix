mod Test.Instance.Eq.Constraint {

    trait Producer[p] {
        type Elm: Type
        type Aef: Eff
        pub def produce(producer: p): Producer.Elm[p] \ Producer.Aef[p]
    }

    trait Consumer[c] {
        type Elm: Type
        type Aef: Eff
        pub def consume(x: Consumer.Elm[c], consumer: c): Unit \ Consumer.Aef[c]
    }

    enum HardcodedProducer[t: Type, ef: Eff](t, Unit -> Unit \ ef)

    instance Producer[HardcodedProducer[t, ef]] {
        type Elm = t
        type Aef = ef
        pub def produce(producer: HardcodedProducer[t, ef]): t \ ef =
            let HardcodedProducer.HardcodedProducer(x, f) = producer;
            f();
            x
    }

    enum HardcodedConsumer[t: Type, ef: Eff](t -> Unit \ ef)

    instance Consumer[HardcodedConsumer[t, ef]] {
        type Elm = t
        type Aef = ef
        pub def consume(x: t, consumer: HardcodedConsumer[t, ef]): Unit \ ef =
            let HardcodedConsumer.HardcodedConsumer(f) = consumer;
            f(x)
    }

    enum ProducerAdapter[t: Type, a: Type, b: Type, ef: Eff](t, a -> b \ ef)

    instance Producer[ProducerAdapter[t, a, b, ef]] with Producer[t] where Producer.Elm[t] ~ a {
        type Elm = b
        type Aef = ef + Producer.Aef[t]
        pub def produce(adapter: ProducerAdapter[t, a, b, ef]): b \ ef + Producer.Aef[t] =
            let ProducerAdapter.ProducerAdapter(producer, f) = adapter;
            f(Producer.produce(producer))
    }

    enum ConsumerAdapter[t: Type, a: Type, b: Type, ef: Eff](t, b -> a \ ef)

    instance Consumer[ConsumerAdapter[t, a, b, ef]] with Consumer[t] where Consumer.Elm[t] ~ a {
        type Elm = b
        type Aef = ef + Consumer.Aef[t]
        pub def consume(x: b, adapter: ConsumerAdapter[t, a, b, ef]): Unit \ ef + Consumer.Aef[t] =
            let ConsumerAdapter.ConsumerAdapter(consumer, f) = adapter;
            Consumer.consume(f(x), consumer)
    }

    enum ProducerTransformer[t: Type, a: Type, ef: Eff](t, a -> a \ ef)

    instance Producer[ProducerTransformer[t, a, ef]] with Producer[t] where Producer.Elm[t] ~ a {
        type Elm = a
        type Aef = ef + Producer.Aef[t]
        pub def produce(transformer: ProducerTransformer[t, a, ef]): Producer.Elm[t] \ ef + Producer.Aef[t] =
            let ProducerTransformer.ProducerTransformer(producer, f) = transformer;
            f(Producer.produce(producer))
    }

    enum ConsumerTransformer[t: Type, a: Type, ef: Eff](t, a -> a \ ef)

    instance Consumer[ConsumerTransformer[t, a, ef]] with Consumer[t] where Consumer.Elm[t] ~ a {
        type Elm = Consumer.Elm[t]
        type Aef = ef + Consumer.Aef[t]
        pub def consume(x: Consumer.Elm[t], transformer: ConsumerTransformer[t, a, ef]): Unit \ ef + Consumer.Aef[t] =
            let ConsumerTransformer.ConsumerTransformer(consumer, f) = transformer;
            Consumer.consume(f(x), consumer)
    }

}
