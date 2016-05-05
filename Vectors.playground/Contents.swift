//: Linear Algebra operations

import UIKit

enum AngleType {
    case Radian, Degrees
}

let significantDigits: Double = 1000 // i.e. 1000 = 0.000

struct Vector: Equatable {
    var coordinate: [Double] = []
    var count: Int {
        get {
            return coordinate.count
        }
    }

    init(coordinates: Double...) {
        for coord in coordinates {
            self.coordinate.append(coord)
        }
    }

    init(coordinates: [Double]) {
        self.coordinate = coordinates
    }

    init() {
        print("no coordinates given")
    }

    func printCoord() {
        print("Vector: \(coordinate)")
    }

    static func sameNumberOfElements(vectorOne: Vector, vectorTwo: Vector) -> Bool {
        let sameNumber = vectorOne.count == vectorTwo.count
        if sameNumber == false {
            print("Different number of elements in coordinates")
        }
        return sameNumber
    }

    static func checkCrossProduct(vector: Vector) -> Vector? {

        //  Add z=0 coordinate if only 2 coordinates given in vector
        if vector.count == 2 {

            var convertedVector = vector.coordinate
            convertedVector.append(0)
            return Vector(coordinates: convertedVector)
        }
        else if vector.count == 3 {

            return vector
        }
        else {
            print("Cross product must only have 2 or 3 dimensions")
            return nil
        }
    }

    //  Magnitude of Vector

    static func magnitude(vector: Vector) -> Double {
        let sumOfSquares = vector.coordinate.map() { $0 * $0 } //  Squares
            .reduce(0) { $0 + $1 } //   Sum
        return sqrt(sumOfSquares)
    }

    //  Direction of Vector (i.e. Normalization)

    static func direction(vector: Vector) -> Vector? {
        guard vector.coordinate.reduce(0, combine: +) != 0 else {
            print("Cannot normalise a zero vector")
            return nil
        }

        let unitVector = 1/magnitude(vector)
        return unitVector * vector
    }

    //  Dot products

    static func dotProduct(vectorOne: Vector, vectorTwo: Vector) -> Double? {
        guard Vector.sameNumberOfElements(vectorOne, vectorTwo: vectorTwo) else {
            return nil
        }

        let multipliedVectors: [Double] = vectorOne.coordinate.map() {
            let index = vectorOne.coordinate.indexOf($0)!
            return $0 * vectorTwo.coordinate[index]
        }

        return multipliedVectors.reduce(0) { $0! + $1 }
    }

    //  Angles between vectors / Theta

    static func angleBetweenVectors(vectorOne: Vector, vectorTwo: Vector, angleType: AngleType) -> Double? {
        guard Vector.sameNumberOfElements(vectorOne, vectorTwo: vectorTwo) else {
            return nil
        }

        guard vectorOne.coordinate.reduce(0, combine: +) != 0 &&
            vectorTwo.coordinate.reduce(0, combine: +) != 0 else {
            print("Cannot find angle which includes a zero vector")
            return nil
        }

        let vectorDotProducts = dotProduct(vectorOne, vectorTwo: vectorTwo)
        let magnitudeDotProduct = magnitude(vectorOne) * magnitude(vectorTwo)

        let radians = acos(vectorDotProducts!/magnitudeDotProduct)
        if angleType == AngleType.Radian {
            return radians
        } else {
            return radians * (180/M_PI) //  Degrees
        }
    }

    //  Parallel vectors

    static func areVectorsParallel(vectorOne: Vector, vectorTwo: Vector) -> (isParallel: Bool, scalar: Double?) {
        guard Vector.sameNumberOfElements(vectorOne, vectorTwo: vectorTwo) else {
            return (isParallel: false, scalar: nil)
        }

        let multipleCoordinate: [Double] = vectorOne.coordinate.map() {
            let index = vectorOne.coordinate.indexOf($0)!
            let valueToCompare = vectorTwo.coordinate[index]
            return $0 / valueToCompare
        }

        let average = multipleCoordinate.reduce(0, combine: +) / Double(multipleCoordinate.count)

        //  If average value is the same as the first value, then all values in array share the same scalar multiple
        if average == multipleCoordinate.first! {
            return (isParallel: true, scalar: average)
        } else {
            return (isParallel: false, scalar: nil)
        }
    }

    //  Projection vector

    static func projectionVector(toVector: Vector, baseVector: Vector) -> Vector? {
        guard Vector.sameNumberOfElements(toVector, vectorTwo: baseVector) else {
            return nil
        }

        if let normalisationOfBaseVector = direction(baseVector),
            let dotProductOfVectorAndBaseVector = dotProduct(toVector, vectorTwo: normalisationOfBaseVector) {
            return dotProductOfVectorAndBaseVector * normalisationOfBaseVector
        } else {
            return nil
        }
    }

    //  Orthogonal vectors

    static func areVectorsOrthogonal(vectorOne: Vector, vectorTwo: Vector) -> Bool? {
        guard Vector.sameNumberOfElements(vectorOne, vectorTwo: vectorTwo) else {
            return nil
        }

        if let vectorDotProduct = dotProduct(vectorOne, vectorTwo: vectorTwo) {
            //  Need to round otherwise the result includes up to e^-14 digits!
            return round(significantDigits * vectorDotProduct) / significantDigits == 0 ? true : false
        } else {
            return false
        }
    }

    static func orthogonalVector(toVector: Vector, baseVector: Vector) -> Vector? {
        guard Vector.sameNumberOfElements(toVector, vectorTwo: baseVector) else {
            return nil
        }

        if let projVector = projectionVector(toVector, baseVector: baseVector) {
            return toVector - projVector
        } else {
            return nil
        }
    }

    //  Cross products

    static func crossProduct(vectorOne: Vector, vectorTwo: Vector) -> Vector? {
        guard Vector.sameNumberOfElements(vectorOne, vectorTwo: vectorTwo) else {
            return nil
        }

        let validatedVectorOne = checkCrossProduct(vectorOne)
        let validatedVectorTwo = checkCrossProduct(vectorTwo)

        guard validatedVectorOne != nil && validatedVectorTwo != nil else {
            return nil
        }

        let v1 = validatedVectorOne!.coordinate, v2 = validatedVectorTwo!.coordinate
        let x = 0, y = 1, z = 2

        let x1 = v1[x], y1 = v1[y], z1 = v1[z]
        let x2 = v2[x], y2 = v2[y], z2 = v2[z]

        let elementOne = (y1 * z2) - (y2 * z1)
        let elementTwo = -((x1 * z2) - (x2 * z1))
        let elementThree = (x1 * y2) - (x2 * y1)

        let crossProd = [elementOne, elementTwo, elementThree]

        return Vector(coordinates: crossProd)
    }

    static func areaOfParallelogram(crossProd: Vector) -> Double? {
        let validatedCrossProd = checkCrossProduct(crossProd)
        guard validatedCrossProd != nil else {
            return nil
        }

        let sumOfSquares = validatedCrossProd!.coordinate.map() { $0 * $0 }.reduce(0, combine: + )
        return sqrt(sumOfSquares)
    }

    static func areaOfTriangle(crossProd: Vector) -> Double? {
        let validatedCrossProd = checkCrossProduct(crossProd)
        guard validatedCrossProd != nil else {
            return nil
        }

        return areaOfParallelogram(validatedCrossProd!)! / 2
    }

}

//  Equatable

func ==(lhs: Vector, rhs: Vector) -> Bool {
    guard Vector.sameNumberOfElements(lhs, vectorTwo: rhs) else {
        return false
    }

    for value in lhs.coordinate {
        let index = lhs.coordinate.indexOf(value)!
        if value != rhs.coordinate[index] {
            return false
        }
    }

    return true
}

//  Arithmetic

func +(lhs: Vector, rhs: Vector) -> Vector? {
    guard Vector.sameNumberOfElements(lhs, vectorTwo: rhs) else {
        return nil
    }

    let newVector: [Double] = lhs.coordinate.map() {
        let index = lhs.coordinate.indexOf($0)!
        return $0 + rhs.coordinate[index]
    }

    return Vector(coordinates: newVector)
}

func -(lhs: Vector, rhs: Vector) -> Vector? {
    guard Vector.sameNumberOfElements(lhs, vectorTwo: rhs) else {
        return nil
    }

    let newVector: [Double] = lhs.coordinate.map() {
        let index = lhs.coordinate.indexOf($0)!
        return $0 - rhs.coordinate[index]
    }

    return Vector(coordinates: newVector)
}

//  Scalar Multiplication

func *(lhs: Double, rhs: Vector) -> Vector {

    let newVector: [Double] = rhs.coordinate.map() {
        return $0 * lhs
    }

    return Vector(coordinates: newVector)
}

func *(lhs: Vector, rhs: Double) -> Vector {
    return rhs * lhs
}


//
//  Challenges
//

//  Basic vector arithmetic

//let coordinateOne = Vector(coordinates: 2, -2)
//let coordinateTwo = Vector(coordinates: -1.129, 2.111)
//(coordinateOne + coordinateTwo)?.printCoord()
//(coordinateOne * 2).printCoord()

//let coordinateThree = Vector(coordinates: -0.211, 7.437)
//let coordinateFour = Vector(coordinates: 8.813, -1.331, -6.247)
//Vector.magnitude(coordinateThree)
//Vector.magnitude(coordinateFour)

//let coordinateFive = Vector(coordinates: 5.581, -2.136)
//let coordinateSix = Vector(coordinates: 1.996, 3.108, -4.554)
//Vector.direction(coordinateFive)!.printCoord()
//Vector.direction(coordinateSix)!.printCoord()

//  Dot products

//let coordinateSeven = Vector(coordinates: 7.887, 4.138)
//let coordinateEight = Vector(coordinates: -8.802, 6.776)
//Vector.dotProduct(coordinateSeven, vectorTwo: coordinateEight)

//let coordinateNine = Vector(coordinates: -5.955, -4.904, -1.874)
//let coordinateTen = Vector(coordinates: -4.496, -8.755, 7.103)
//Vector.dotProduct(coordinateNine, vectorTwo: coordinateTen)

//  Angles / Thetas
//let coordinateEleven = Vector(coordinates: 3.183, -7.627)
//let coordinateTwelve = Vector(coordinates: -2.668, 5.319)
//Vector.angleBetweenVectors(coordinateEleven, vectorTwo: coordinateTwelve, angleType: .Radian)

//let coordinateThirteen = Vector(coordinates: 7.35, 0.221, 5.188)
//let coordinateFourteen = Vector(coordinates: 2.751, 8.259, 3.985)
//Vector.angleBetweenVectors(coordinateThirteen, vectorTwo: coordinateFourteen, angleType: .Degrees)

//  Parallel & Orthogonals

//let pVectorOne = Vector(coordinates: -7.579, -7.88)
//let pVectorTwo = Vector(coordinates: 22.737, 23.64)
//Vector.areVectorsParallel(pVectorOne, vectorTwo: pVectorTwo)
//Vector.areVectorsOrthogonal(pVectorOne, vectorTwo: pVectorTwo)

//let pVectorThree = Vector(coordinates: -2.029, 9.97, 4.172)
//let pVectorFour = Vector(coordinates: -9.231, -6.639, -7.245)
//Vector.areVectorsParallel(pVectorThree, vectorTwo: pVectorFour)
//Vector.areVectorsOrthogonal(pVectorThree, vectorTwo: pVectorFour)

//let pVectorFive = Vector(coordinates: -2.328, -7.284, -1.214)
//let pVectorSix = Vector(coordinates: -1.821, 1.072, -2.94)
//Vector.areVectorsParallel(pVectorFive, vectorTwo: pVectorSix)
//Vector.areVectorsOrthogonal(pVectorFive, vectorTwo: pVectorSix)

//let pVectorSeven = Vector(coordinates: 2.118, 4.827)
//let pVectorEight = Vector(coordinates: 0, 0)
//Vector.areVectorsParallel(pVectorSeven, vectorTwo: pVectorEight)
//Vector.areVectorsOrthogonal(pVectorSeven, vectorTwo: pVectorEight)

//  Projection & Orthogonal Vectors

//let vectorOne = Vector(coordinates: 3.039, 1.879)
//let baseVectorOne = Vector(coordinates: 0.825, 2.036)
//Vector.projectionVector(vectorOne, baseVector: baseVectorOne)?.printCoord()

//let vectorTwo = Vector(coordinates: -9.88, -3.264, -8.159)
//let baseVectorTwo = Vector(coordinates: -2.155, -9.353, -9.473)
//Vector.orthogonalVector(vectorTwo, baseVector: baseVectorTwo)?.printCoord()

//let vectorThree = Vector(coordinates: 3.009, -6.172, 3.692, -2.51)
//let baseVectorThree = Vector(coordinates: 6.404, -9.144, 2.759, 8.718)
//Vector.projectionVector(vectorThree, baseVector: baseVectorThree)?.printCoord()
//Vector.orthogonalVector(vectorThree, baseVector: baseVectorThree)?.printCoord()

//  Cross Vectors and areas

//let crossVectorOne = Vector(coordinates: 8.462, 7.893, -8.187)
//let crossVectorTwo = Vector(coordinates: 6.984, -5.975, 4.778)
//Vector.crossProduct(crossVectorOne, vectorTwo: crossVectorTwo)?.printCoord()
//
//let crossVectorThree = Vector(coordinates: -8.987, -9.838, 5.031)
//let crossVectorFour = Vector(coordinates: -4.268, -1.861, -8.866)
//let crossProductOne = Vector.crossProduct(crossVectorThree, vectorTwo: crossVectorFour)!
//Vector.areaOfParallelogram(crossProductOne)
//
//let crossVectorFive = Vector(coordinates: 1.5, 9.547, 3.691)
//let crossVectorSix = Vector(coordinates: -6.007, 0.124, 5.772)
//let crossProductTwo = Vector.crossProduct(crossVectorFive, vectorTwo: crossVectorSix)!
//Vector.areaOfTriangle(crossProductTwo)
