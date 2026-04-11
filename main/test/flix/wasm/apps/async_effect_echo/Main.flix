mod Api {

    eff HostEcho {
        def echo(s: String): String
    }

    @Export
    pub def callEcho(s: String): String \ HostEcho =
        HostEcho.echo(s)

}
