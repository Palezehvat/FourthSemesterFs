module TestsTasks

open Program.EvenNumber
open Program.BinaryTree
open Program.ParsingTree
open Program.PrimeSeq

open NUnit.Framework

[<Test>]
let EvenNumbersShouldSelectedCorrectlyByMap() =
    Assert.That(countingEvenNumbersByMap [1; 2; 3; 4; 5], Is.EqualTo(2))

[<Test>]
let EvenNumbersShouldSelectedCorrectlyInEmptyListByMap() =
    Assert.That(countingEvenNumbersByMap [], Is.EqualTo(0))

[<Test>]
let EvenNumbersShouldSelectedCorrectlyByFilter() =
    Assert.That(countingEvenNumbersByFilter [1; 2; 3; 4; 5], Is.EqualTo(2))
[<Test>]

let EvenNumbersShouldSelectedCorrectlyInEmptyListByFilter() =
    Assert.That(countingEvenNumbersByFilter [], Is.EqualTo(0))

[<Test>]
let EvenNumbersShouldSelectedCorrectlyByFold() =
    Assert.That(countingEvenNumbersByFold [1; 2; 3; 4; 5], Is.EqualTo(2))

[<Test>]
let EvenNumbersShouldSelectedCorrectlyInEmptyListByFold() =
    Assert.That(countingEvenNumbersByFold [], Is.EqualTo(0))

[<Test>]
let BinaryTreesShouldGenerateCorrectly() = 
    let tree = BinaryTree(1, BinaryTree.Value 2, BinaryTree(3, BinaryTree.Value 4, BinaryTree.Value 5))
    let newTree = mapTree ((+) 1) tree
    Assert.That(newTree, Is.EqualTo(BinaryTree(2, BinaryTree.Value 3, BinaryTree(4, BinaryTree.Value 5, BinaryTree.Value 6))))

[<Test>]
let TestParsingTreeShouldWorkCorrectlyWithOneOperation() =
    let tree = Add(ParsingTree.Value 2, ParsingTree.Value 3)
    Assert.That(evaluateParsingTree tree, Is.EqualTo(5))

[<Test>]
let TestParsingTreeShouldWorkCorrectlyWithSeveralOperations() =
    let tree = Add(ParsingTree.Value 2, Add(ParsingTree.Value 3, ParsingTree.Value 4))
    Assert.That(evaluateParsingTree tree, Is.EqualTo(9))

[<Test>]
let ParsingTreeShouldWorkCorrectlyWithDifferentActions() =
    let tree = Multiply(Add(ParsingTree.Value 5, ParsingTree.Value 10), ParsingTree.Value 2)
    Assert.That(evaluateParsingTree tree, Is.EqualTo(30))

[<Test>]
let PrimeSeqShouldGenerateCorrectly() = 
    let expectedPrimes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
    let actualPrimes = Seq.take 10 primeSequence |> Seq.toList

    Assert.AreEqual(expectedPrimes, actualPrimes)