/// <summary>
/// Counts the factorial of a number. -1 If a negative number is passed
/// </summary>
/// <param name="n">The number by which the factorial is calculated</param>
let factorial n =
    if n < 0 then -1
    else
        let rec helpFactorial n acc =
            if n = 1 || n = 0 then acc
            else helpFactorial (n - 1) (n * acc)
        helpFactorial n 1

/// <summary>
/// Calculates the fibonacci number by its position. -1 If an impossible position is passed
/// </summary>
/// <param name="n">The position of the Fibonacci number</param>
let fibonacci n =
    if n <= 0 then -1
    else
        let rec helpFibonacci n firstNumber secondNumber =
            if n = 1 || n = 2 then secondNumber
            else helpFibonacci (n - 1) secondNumber (firstNumber + secondNumber)
        helpFibonacci n 1 1

/// <summary>
/// Reverse the list
/// </summary>
/// <param name="list">The list that needs to be reversed</param>
let reverse list =
    let rec helpReverse list acc =
        match list with
        | [] -> acc
        | head :: tail -> helpReverse tail (head :: acc)
    helpReverse list []

/// <summary>
/// Creates a list with powers of two, as follows [2^n; 2^(n + 1); ...; 2^(n + m)]
/// </summary>
/// <param name="n">Initial degree</param>
/// <param name="m">Delta</param>
let returnListWithDegreesOfTwo n m =
    let temple = 2.0 ** n
    let rec helpReturnListWithDegreesOfTwo n m acc list =
        let storage = acc * 2.0
        if m = 0 then reverse list
        else helpReturnListWithDegreesOfTwo n (m - 1) storage (storage :: list)
    helpReturnListWithDegreesOfTwo n m temple [temple]

/// <summary>
/// Returns the position of the first occurrence of a number in the list. -1 If the number was not in the list
/// </summary>
/// <param name="list">The original list</param>
/// <param name="number">The required number</param>
let find list number =
    let rec helpFind list number acc =
        match list with
        | [] -> -1
        | head :: tail ->
            if head = number then acc
            else helpFind tail number (acc + 1)
    helpFind list number 0