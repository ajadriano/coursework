import org.scalatest.FlatSpec
import collection.mutable.Stack

class Test extends FlatSpec {
  
  "generateCodeBlock" should "generate the correct code block" in {
    var coder = new Coder("")
    assert(coder.codeBlock.deep == Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')).deep)
    coder = new Coder("ffrroossttiieeess")
    assert(coder.codeBlock.deep == Array(Array('f', 'r', 'o', 's', 't'), Array('i', 'e', 'a', 'b', 'c'), Array('d', 'g', 'h', 'k', 'l'), Array('m', 'n', 'p', 'q', 'u'), Array('v', 'w', 'x', 'y', 'z')).deep) 
    coder = new Coder("F^&*)_E*((*^D)&&*C(  B  (*) A")
    assert(coder.codeBlock.deep == Array(Array('f', 'e', 'd', 'c', 'b'), Array('a', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')).deep)
  }
  
  "adjustDoubleLetters" should "insert an x between a double pair" in {
    var coder = new Coder("")
    var output = coder.adjustDoubleLetters("aa").mkString;
    assert(output == "axaz")
    output = coder.adjustDoubleLetters("aabcaa").mkString;
    assert(output == "axabcaaz")
    output = coder.adjustDoubleLetters("aabcaad").mkString;
    assert(output == "axabcaad")
    output = coder.adjustDoubleLetters("bbbbbbbbb").mkString;
    assert(output == "bxbxbxbxbxbxbxbxbz")
  }
  
  "adjustDoubleLetters" should "insert an q between xx" in {
    var coder = new Coder("")
    var output = coder.adjustDoubleLetters("xx").mkString;
    assert(output == "xqxz")
    output = coder.adjustDoubleLetters("xxbcxx").mkString;
    assert(output == "xqxbcxxz")
    output = coder.adjustDoubleLetters("xxbcxxd").mkString;
    assert(output == "xqxbcxxd")
    output = coder.adjustDoubleLetters("xxxxx").mkString;
    assert(output == "xqxqxqxqxz")
  }
  
  "decryptDoubleLetters" should "parse double letters correctly" in {
    var coder = new Coder("")
    var output = coder.decryptDoubleLetters(List(new Tuple2('a', 'x'), new Tuple2('a', 'z'))).mkString
    assert(output == "aa")
    output = coder.decryptDoubleLetters(List(new Tuple2('a', 'x'), new Tuple2('a', 'b'), new Tuple2('c', 'a'), new Tuple2('a', 'z'))).mkString
    assert(output == "aabcaa")
    output = coder.decryptDoubleLetters(List(new Tuple2('a', 'x'), new Tuple2('a', 'b'), new Tuple2('c', 'a'), new Tuple2('a', 'd'))).mkString
    assert(output == "aabcaad")
    output = coder.decryptDoubleLetters(List(new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'x'), new Tuple2('b', 'z'))).mkString
    assert(output == "bbbbbbbbb")
    output = coder.decryptDoubleLetters(List(new Tuple2('x', 'q'), new Tuple2('x', 'z'))).mkString
    assert(output == "xx")
    output = coder.decryptDoubleLetters(List(new Tuple2('x', 'q'), new Tuple2('x', 'b'), new Tuple2('c', 'x'), new Tuple2('x', 'z'))).mkString
    assert(output == "xxbcxx")
    output = coder.decryptDoubleLetters(List(new Tuple2('x', 'q'), new Tuple2('x', 'b'), new Tuple2('c', 'x'), new Tuple2('x', 'd'))).mkString
    assert(output == "xxbcxxd")
  }
  
  "transformPlainText" should "transform text into tuples" in {
    var coder = new Coder("")
    var output = coder.transformPlainText("aa")
    assert(output == List(new Tuple2('a', 'x'), new Tuple2('a', 'z')))
    output = coder.transformPlainText("xx")
    assert(output == List(new Tuple2('x', 'q'), new Tuple2('x', 'z')))
  }
  
  "transformSecretText" should "transform text into tuples" in {
    var coder = new Coder("")
    var output = coder.transformSecretText("ab*&^&*^  cd")
    assert(output == List(new Tuple2('a', 'b'), new Tuple2('c', 'd')))
    try{
      output = coder.transformSecretText("")
      assert(false, "Exception should have occurred")
    }
    catch
    {
      case arg : IllegalArgumentException => assert(arg.getMessage == "requirement failed: No characters in secret text.") 
      case _ : Throwable => assert(false, "Exception mismatch")
    }
    try{
      output = coder.transformSecretText("abc")
      assert(false, "Exception should have occurred")
    }
    catch
    {
      case arg : IllegalArgumentException => assert(arg.getMessage == "requirement failed: Should have even number of characters.") 
      case _ : Throwable => assert(false, "Exception mismatch")
    }
  }
  
  "getLocation" should "return location of character in matrix" in {
    var coder = new Coder("")
    var output = coder.getLocation(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 'e')
    assert(output == Tuple2(0,4))
    output = coder.getLocation(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 'z')
    assert(output == Tuple2(4,4))
  }
  
  "encryptTuple" should "convert column row tuple to encrypted state" in {
    var coder = new Coder("")
    var output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('a', 'k'))
    assert(output == Tuple2('e','f'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('e', 'f'))
    assert(output == Tuple2('a','k'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('c', 'h'))
    assert(output == Tuple2('h','n'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('s', 'x'))
    assert(output == Tuple2('x','c'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('v', 'a'))
    assert(output == Tuple2('a','f'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('l', 'm'))
    assert(output == Tuple2('m','n'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('o', 'p'))
    assert(output == Tuple2('p','l'))
    output = coder.encryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('k', 'f'))
    assert(output == Tuple2('f','g'))
  }
  
  "decryptTuple" should "convert column row tuple to decrypted state" in {
    var coder = new Coder("")
    var output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('e', 'f'))
    assert(output == Tuple2('a','k'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('a', 'k'))
    assert(output == Tuple2('e','f'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('h', 'n'))
    assert(output == Tuple2('c','h'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('x', 'c'))
    assert(output == Tuple2('s','x'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('a', 'f'))
    assert(output == Tuple2('v','a'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('m', 'n'))
    assert(output == Tuple2('l','m'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('p', 'l'))
    assert(output == Tuple2('o','p'))
    output = coder.decryptTuple(Array(Array('a', 'b', 'c', 'd', 'e'), Array('f', 'g', 'h', 'i', 'k'), Array('l', 'm', 'n', 'o', 'p'), Array('q', 'r', 's', 't', 'u'), Array('v', 'w', 'x', 'y', 'z')), 
        Tuple2('f', 'g'))
    assert(output == Tuple2('k','f'))
  }
  
  "transformOutput" should "transform list of characters into 5 character blocks" in {
    var coder = new Coder("")
    var output = coder.transformOutput(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))
    assert(output == "abcde fghi")
  }
  
  "encode" should "encode plain text" in {
    var coder = new Coder("pennsylvania")
    var output = coder.encode("An anonymous reader sends word of a proof-of-concept Google Chrome browser extension that steals users’ login details. The developer, Andreas Grech, says that he is trying to raise awareness about security among end users, and therefore chose Chrome as a test-bed because of its reputation as the safest browser.")
    assert(output == "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\ndckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\ndymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\nbonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\ndckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\nkqxny m")
  }
  
  "decode" should "decode secret text" in {
    var coder = new Coder("pennsylvania")
    var output = coder.decode("fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\ndckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\ndymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\nbonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\ndckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\nkqxny m")
    assert(output == "anano nymou sread ersen dswor dofap roofo fconc eptgo oglec\nhrome brows erext ensio nthat steal suser slogi ndeta ilsth\nedeve loper andre asgre chsay sthat heist rying torai seawa\nrenes sabou tsecu ritya monge nduse rsand there forec hosec\nhrome asate stbed becau seofi tsrep utati onast hesaf estbr\nowser")
  }
}