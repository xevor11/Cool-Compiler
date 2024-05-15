import Test.Hspec
import Lexer
import Tokens

tokenizeString :: String -> IO [Token]
tokenizeString input = do
    runLexer input

inputCode :: String
inputCode = unlines
    [ "class Enumeration() extends IO() {"
    , "  /**"
    , "   * Returns false as the default implementation."
    , "   */"
    , "  def hasNext(): Boolean = false;"
    , ""
    , "  /**"
    , "   * Aborts the program as there is no correct default action without backing data."
    , "   */"
    , "  def next(): Any = abort(\"No elements in the enumeration\");"
    , "}"
    , ""
    , "class Vector() {"
    , "  var arr: ArrayAny = new ArrayAny(10);"
    , "  var itemCount: Int = 0;"
    , ""
    , "  def getArr(): ArrayAny = arr;"
    , "  /**"
    , "   * Returns the number of items in the vector."
    , "   */"
    , "  def size(): Int = itemCount;"
    , ""
    , "  /**"
    , "   * Adds the given Any value to the end of the vector."
    , "   */"
    , "  def add(value: Any): Unit = {"
    , "    if (itemCount == arr.length()) {"
    , "        arr = arr.resize(arr.length() * 2)"
    , "    } else ();"
    , ""
    , "    arr.set(itemCount, value);"
    , "    itemCount = itemCount + 1"
    , "  };"
    , ""
    , "  /**"
    , "   * Returns a new VectorEnumeration instance backed by this vector."
    , "   */"
    , "  def elements(): VectorEnumeration = new VectorEnumeration(this);"
    , ""
    , "  /**"
    , "   * Removes all items from the vector."
    , "   */"
    , "  def clear(): Unit = {"
    , "    itemCount = 0"
    , "  };"
    , "}"
    , ""
    , "class VectorEnumeration(var elements: Vector) extends Enumeration() {"
    , "  var currentIndex: Int = 0;"
    , "  /**"
    , "   * Returns true if a call to next will succeed."
    , "   */"
    , "  override def hasNext(): Boolean = currentIndex < elements.size();"
    , ""
    , "  /**"
    , "   * Returns the next element in the enumeration or aborts the program if no such element exists."
    , "   */"
    , "  override def next(): Any = {"
    , "    if (hasNext()) {"
    , "      var array : ArrayAny = elements.getArr();"
    , "      var element : Any = array.get(currentIndex);"
    , "      currentIndex = currentIndex + 1;"
    , "      element"
    , "    } else {"
    , "      abort(\"No more elements in the enumeration\")"
    , "    }"
    , "  };"
    , "}"
    ]

main :: IO ()
main = do
    tokens <- tokenizeString inputCode
    print tokens

