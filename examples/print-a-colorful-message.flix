/// Construct colorful messages.
def main(): Unit \ IO =
  let s1 = "You can print message with " + Chalk.red("colored text");
  let s2 = " or " + Chalk.bgBlue("background") + ".";
  println(s1+s2);

  let s3 = Chalk.bgYellow(Chalk.magenta("This message has both magenta text and yellow background."));
  println(s3);

  let s4 = Chalk.black("This is a ") :: Chalk.red("c") :: Chalk.green("o") ::
            Chalk.yellow("l") :: Chalk.blue("o") :: Chalk.magenta("r") ::
            Chalk.cyan("f") :: Chalk.greenBright("u") :: Chalk.blueBright("l") ::
            Chalk.black(" message.") :: Nil;
  let s5 = List.map(s -> Chalk.bgWhite(s), s4);
  List.forEach(s -> println(s), s5);
  println("");

  let s6 = Chalk.bold("This message is bold.");
  let s7 = Chalk.hex("#b891eb", " And this is a custom hex color.");
  println(s6 + s7)
