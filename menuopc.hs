main = do
    putStrLn("---------------------------------")
    putStrLn("Menu principal \n\n 1-Fibonacci \n 2-Numeros del 1 al 10 \n 3-Factorial \n 4-Descedendte \n 5-Palindromos \n 6-Calculadora \n 7-Salir")
    n <- getLine
    putStrLn("---------------------------------")
    putStrLn("Mostrando \n")
    let nInt = read n::Int
    casos nInt

casos n = do

        case n of
             1 -> fibonacci
             2 -> numeros 1 
             3 -> factorial
             4 -> desaparece 0
             5 -> palindromo
             6 -> calculadora
             7-> print("salir")
             _ -> print("Opcion no Disponible")


--1 serie fibonacci
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x - 1) + fibo (x - 2)

fibonacci = do
    putStrLn("Ingresa la posicion")
    ciclo <- getLine
    print(map (fibo) [0..(read (ciclo))])
    putStrLn("\n")
    main





--2 numeros del 1 al 10 ready add main

numeros n = do
    if n <= 10
    then do
    print n
    numeros (n + 1)
    else do
        putStrLn("") 
        main

--3 factorial ready add main


fact :: Int -> Int
fact 0=1
fact n = n * fact(n-1)

factorial = do 
  
    putStrLn("ingresa valor a calcular")
    val <-getLine
    putStr("el resultado es ")
    print( fact(read(val)))
    main
   



--4 Desaparece numeros

desaparece n = do 
    let datos = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    if n <= 10
    then do 
    print(take (11-n) datos)

    desaparece(n+1)
    --print
 

    else do 
        putStrLn("")
        main
  


-- 5 palindromos --ready add main
palindromo = do
    putStrLn("ingresa la palabra")
    pal <-getLine
    if pal == reverse pal
    then do 
    putStrLn("\nEs Palindromo")
    main
    else do
        putStrLn("no es palindromo")
        main

--6 Calculadora
calculadora = do

    putStrLn("-----------------------------\n")
    putStrLn("Ingresa el numero de tu operacion a realizar: \n 1-suma \n 2-resta \n 3-multiplicacion \n 4-division \n 5-salir")
    op <- getLine
    putStrLn("-----------------------------\n")
    let opInt = read op :: Int

    menu opInt


menu op = do


        case op of
             1 -> suma
             2 -> resta   
             3 -> multiplicacion
             4 -> division
             5 -> main
             _ -> print("Esta opcion no esta disponible")          




suma = do 
    
    putStrLn ("Ingresa el valor del numero 1")
    uno <- getLine
    putStrLn ("Ingresa el valor del numero 2")
    dos <- getLine
    putStr("Es el resultado es: ")
    print(read(uno)+read(dos))
    calculadora
resta = do 
    putStrLn ("Ingresa el valor del numero 1")
    uno <- getLine
    putStrLn ("Ingresa el valor del numero 2")
    dos <- getLine
    putStr("Es el resultado es: ")
    print(read(uno)-read(dos))
    calculadora
multiplicacion = do 
    putStrLn ("Ingresa el valor del numero 1")
    uno <- getLine
    putStrLn ("Ingresa el valor del numero 2")
    dos <- getLine
    putStr("Es el resultado es: ")
    print(read(uno)*read(dos))
    calculadora
division = do
    putStrLn ("Ingresa el valor del numero 1")
    uno <- getLine
    putStrLn ("Ingresa el valor del numero 2")
    dos <- getLine
    putStr("Es el resultado es: ")
    print(read(uno)/read(dos))
    calculadora

   