module EvenNumber = 
    let isEvenNumber x =
        if x % 2 = 0 then 1 else 0

    let countingEvenNumbersByMap list =
        let newList = List.map isEvenNumber list
        List.sum newList

    let countingEvenNumbersByFilter list = 
        let newList = List.filter (fun x -> x % 2 = 0) list
        List.length newList

    let countingEvenNumbersByFold list =
        List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0 list

module BinaryTree = 
    type BinaryTree<'T> =
        | BinaryTree of 'T * BinaryTree<'T> * BinaryTree<'T>
        | Value of 'T

    let rec mapTree f = function
        | Value v -> Value (f v)
        | BinaryTree (v, left, right) -> BinaryTree (f v, mapTree f left, mapTree f right)

module ParsingTree = 
    type ParsingTree = 
        | Value of int
        | Add of ParsingTree * ParsingTree
        | Subtract of ParsingTree * ParsingTree
        | Multiply of ParsingTree * ParsingTree
        | Divide of ParsingTree * ParsingTree

    let rec evaluateParsingTree tree =
        match tree with
        | Value v -> v
        | Add (left, right) -> evaluateParsingTree left + evaluateParsingTree right
        | Subtract (left, right) -> evaluateParsingTree left - evaluateParsingTree right
        | Multiply (left, right) -> evaluateParsingTree left * evaluateParsingTree right
        | Divide (left, right) -> evaluateParsingTree left / evaluateParsingTree right

module PrimeSeq =
    let rec isPrime n =
        let limit = int(sqrt(double n)) + 1
        let rec checkDivisors divisor =
            if divisor > limit then true
            elif n % divisor = 0 then false
            else checkDivisors (divisor + 1)
        checkDivisors 2

    let primeSequence = Seq.initInfinite ((+) 2) |> Seq.filter isPrime