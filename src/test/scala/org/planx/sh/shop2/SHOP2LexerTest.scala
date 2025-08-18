package org.planx.sh.parsing.shop2

      import scala.io.Source
      import scala.util.parsing.input.CharArrayReader

      object SHOP2LexerTest extends App {
        val lexer = new SHOP2Lexer()

        def testString(input: String): Unit = {
          println(s"\n=== Testing string: '$input' ===")

          val reader = new CharArrayReader(input.toCharArray)
          var currentScanner = new lexer.Scanner(reader)

          var tokenCount = 0
          while (currentScanner.first != lexer.EOF) {
            tokenCount += 1
            println(s"Token $tokenCount:")
            val token = currentScanner.first
            println(s"${token.getClass.getSimpleName} -> '$token '")
            if (token.toString.contains("ErrorToken")) {
              println("ErrorToken encountered. Stopping to prevent infinite loop.")
              return
            }
            currentScanner = currentScanner.rest
          }
        }

        def testFile(filename: String): Unit = {
          println(s"\n=== Testing file: $filename ===")

          val source = Source.fromFile(filename)
          val content = source.mkString
          source.close()

          println(s"Input content:\n$content\n")

          val reader = new CharArrayReader(content.toCharArray)
          val scanner = new lexer.Scanner(reader)

          println("Tokens:")
          var currentScanner = scanner
          var tokenCount = 0

          while (currentScanner.first != lexer.EOF) {
            tokenCount += 1
            val token = currentScanner.first
            println(s"$tokenCount: ${token.getClass.getSimpleName} -> '$token'")
            currentScanner = currentScanner.rest

            if (token.toString.contains("ErrorToken")) {
              println("ErrorToken encountered. Stopping to prevent infinite loop.")
              return
            }
          }

          println(s"\nTotal tokens: $tokenCount")
        }


        testString("?hello-world _my-constant test-name 42.5 -17")
        testString("!task ?variablenName ?_")
        testString("; this is a comment\nhello")
        testString("(defdomain basic)")

        try {
          testFile("C:\\Users\\boike\\IdeaProjects\\sh-translator\\src\\main\\scala\\org\\planx\\sh\\parsing\\shop2\\basicdomain.lisp")
        } catch {
          case e: Exception => println(s"Fehler beim Lesen der Datei: ${e.getMessage}")
        }
      }