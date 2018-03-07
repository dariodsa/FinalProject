module Input where
sayHello :: IO Char
sayHello = do 
           putStrLn "Hello, do you want me to generate dot file press 1, otherwise press 2"
           input <- getChar
           return (input)