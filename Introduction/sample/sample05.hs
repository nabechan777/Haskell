{-
    型
    ・変数は右辺の値から型推論が働くため型を明示的にする必要はない。
    ・型シグネチャを使うことで明示的に示すこともできる。
    ・要素数0のタプルをunitという。意味のある値がないことを示す。
-}
bool_value :: Bool --真偽値
bool_value = True
int_value :: Int --固定長整数
int_value = 123
integer_value :: Integer --多倍長整数
integer_value = 123456789
float_value :: Float --単精度少数点数
float_value = 3.14
double_value :: Double --倍精度浮動小数点数
double_value = 3.141592
char_value :: Char --文字
char_value = 'W'
string_value :: String --文字列
string_value = "Haskell"
tuple_value :: (Int, String) --IntとStringのタプル
tuple_value = (624, "Haskell")
list_value :: [Int] --Intのリスト
list_value = [1, 2, 3]

main = do
    print bool_value
    print int_value
    print integer_value
    print float_value
    print double_value
    print char_value
    print double_value
    print tuple_value
    print list_value
