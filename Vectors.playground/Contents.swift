//: Linear Algebra operations

import UIKit

enum AngleType {
    case Radian, Degrees
}

let significantDigits: Double = 1000 // i.e. 1000 = 0.000

struct Vector: Equatable, CustomStringConvertible {
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

    var description: String {
        return "Vector: \(coordinate)"
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

        } else if vector.count == 3 {

            return vector

        } else {

            print("Cross product between Vectors must only have 2 or 3 dimensions")
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
//            return round(($0 / valueToCompare) * significantDigits) / significantDigits
            return $0 / valueToCompare

        }

        let averageExact = multipleCoordinate.reduce(0, combine: +) / Double(multipleCoordinate.count)
        let average = round(averageExact * significantDigits) / significantDigits

        //  If average value is the same as the first value (within significant digits threshold), then all values in array share the same scalar multiple
        if 0...(1/significantDigits) ~= abs(average) - abs(multipleCoordinate.first!) {
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

class MultiDimensionVector: CustomStringConvertible {
    private var constantTerm = 0.0
    private var basePoint = Vector()
    private var basePointCoords: [Double]
    private var normalVector: Vector
    var dimensions: Int

    var description: String {
        return "\(dimensions)D Vector"
    }

    init(normalVector: Vector, constantTerm: Double?) {

        self.normalVector = normalVector

        dimensions = self.normalVector.count
        var coords: [Double] = []
        for _ in 1...dimensions {
            coords.append(0)
        }
        basePointCoords = coords

        self.constantTerm = constantTerm ?? 0

        setBasePointCoords()
    }

    private func setBasePointCoords() {
        if let initialIndex = self.findFirstNonZeroIndex(normalVector) {

            let initialCoefficient = normalVector.coordinate[initialIndex]
            self.basePointCoords[initialIndex] = constantTerm/initialCoefficient
        }
        self.basePoint = Vector(coordinates: self.basePointCoords)
    }

    private func findFirstNonZeroIndex(normalVector: Vector) -> Int? {

        let firstElement = normalVector.coordinate.filter {
            let rounded = round($0 * significantDigits) / significantDigits
            return rounded != 0
            }.first

        return firstElement != nil ? normalVector.coordinate.indexOf(firstElement!) : nil
    }

    func createStandardForm() -> String {

        if let initialIndex = self.findFirstNonZeroIndex(normalVector) {
            let coordinates = normalVector.coordinate
            let terms: [String] = coordinates.map {
                let isInitialElement = coordinates.indexOf($0) == initialIndex
                var output = ""
                let roundedCoefficent = round($0 * significantDigits) / significantDigits
                if roundedCoefficent < 0 {
                    output += "-"
                } else if roundedCoefficent > 0 && !isInitialElement {
                    output += "+"
                }

                if abs(roundedCoefficent) != 0 {
                    output += "\(abs(roundedCoefficent))"
                }

                return output
            }

            var standardForm = terms.joinWithSeparator(" ")
            let roundedConstant = round(constantTerm * significantDigits) / significantDigits
            standardForm += " = \(roundedConstant)"
            return standardForm
        }

        print("Couldn't createStandardForm")
        return ""
    }

    func isParallelTo(mdVector: MultiDimensionVector) -> (isParallel: Bool, isEqual: Bool) {

        //  Two lines in 2D are parallel if their normal vectors are parallel vectors
        let parallel = Vector.areVectorsParallel(self.normalVector, vectorTwo: mdVector.normalVector)

        if parallel.isParallel {
            //  Two parallel lines are equal if the vector connecting one point on each line is orthogonal to the lines' normal vectors

            //  The vector connecting a point on each line
            let differenceVector = self.basePoint - mdVector.basePoint

            //  Only need to calculate orthogonality for one line, since we know they are both parallel
            return (isParallel: true, isEqual: Vector.areVectorsOrthogonal(self.normalVector, vectorTwo: differenceVector!) ?? false)
        }

        return (isParallel: false, isEqual: false)
    }

    func isIntersectedWith(mdVector: MultiDimensionVector) -> (x: Double, y: Double)? {
        guard isParallelTo(mdVector).isParallel == false else {
            return nil
        }

        //  Ax + By = k1
        //  Cx + Dy = k2

        //  x = (Dk1 - Bk2) / (AD - BC)
        //  y = (-Ck1 + Ak2) / (AD - BC)

        let lineOneStandardForm = self.createStandardForm().componentsSeparatedByString(" ")
        let A = Double(lineOneStandardForm[0])!
        let B = Double(lineOneStandardForm[1])!
        let K1 = Double(lineOneStandardForm[3])!

        let lineTwoStandardForm = mdVector.createStandardForm().componentsSeparatedByString(" ")
        let C = Double(lineTwoStandardForm[0])!
        let D = Double(lineTwoStandardForm[1])!
        let K2 = Double(lineTwoStandardForm[3])!

        let x = ((D * K1) - (B * K2)) / ((A * D) - (B * C))
        let y = ((-C * K1) + (A * K2)) / ((A * D) - (B * C))
        
        return (x: x, y: y)
    }
}

//  Equatable

func ==(lhs: MultiDimensionVector, rhs: MultiDimensionVector) -> Bool {
    guard lhs.dimensions == rhs.dimensions else {
        return false
    }

    if lhs.createStandardForm() == rhs.createStandardForm() {
        return true
    } else {
        return false
    }
}

class Line: MultiDimensionVector {
    init(x: Double, y: Double, constant: Double) {
        super.init(normalVector: Vector(coordinates: x, y), constantTerm: constant)
    }

    static func describe(lineOne: Line, _ lineTwo: Line) -> String {
        let isParallel = lineOne.isParallelTo(lineTwo)

        if isParallel.isParallel && isParallel.isEqual {
            return "Lines are the same line (equal and parallel)"
        }

        if isParallel.isParallel && isParallel.isEqual == false {
            return "Lines are parallel, but not equal (no intersection)"
        }

        if isParallel.isParallel == false {
            var string = ""
            if isParallel.isEqual {
                string = "Lines are equal and "
            }

            if let isIntersected = lineOne.isIntersectedWith(lineTwo) {
                string += "Lines intersect at (\(round(isIntersected.x * significantDigits) / significantDigits), \(round(isIntersected.y * significantDigits) / significantDigits))"
            }

            return string
        }

        return ""
    }
}

class Plane: MultiDimensionVector {
    init(x: Double, y: Double, z: Double, constant: Double) {
        super.init(normalVector: Vector(coordinates: x, y, z), constantTerm: constant)
    }

    static func describe(planeOne: Plane, _ planeTwo: Plane) -> String {
        let isParallel = planeOne.isParallelTo(planeTwo)

        if isParallel.isParallel && isParallel.isEqual {
            return "Planes are the same plane (equal and parallel)"
        }

        if isParallel.isParallel && isParallel.isEqual == false {
            return "Planes are parallel, but not equal (no intersection)"
        }

        if isParallel.isParallel == false {
            var string = "Planes are not parallel"
            if isParallel.isEqual {
                string = "Planes are equal and "
            } else {
                string += " and are not equal"
            }

            //  TODO: Plane intersection

            return string
        }

        return ""
    }
}


//
//  Lines
//

let line1 = Line(x: 4.046, y: 2.836, constant: 1.21)
let line2 = Line(x: 10.115, y: 7.09, constant: 3.025)
Line.describe(line1, line2)

let line3 = Line(x: 7.204, y: 3.182, constant: 8.68)
let line4 = Line(x: 8.172, y: 4.114, constant: 9.883)
Line.describe(line3, line4)

let line5 = Line(x: 1.182, y: 5.562, constant: 6.744)
let line6 = Line(x: 1.773, y: 8.343, constant: 9.525)
Line.describe(line5, line6)

//
//  Planes
//

let plane1 = Plane(x: -0.412, y: 3.806, z: 0.728, constant: -3.46)
let plane2 = Plane(x: 1.03, y: -9.515, z: -1.82, constant: 8.65)
Plane.describe(plane1, plane2)

let plane3 = Plane(x: 2.611, y: 5.528, z: 0.283, constant: 4.6)
let plane4 = Plane(x: 7.715, y: 8.306, z: 8.306, constant: 3.76)
Plane.describe(plane3, plane4)

let plane5 = Plane(x: -7.926, y: 8.625, z: -7.217, constant: -7.952)
let plane6 = Plane(x: -2.642, y: 2.875, z: -2.404, constant: -2.443)
Plane.describe(plane5, plane6)


class LinearSystem {
    enum Message: String {
        case AllPlanesMustBeInSameDimension = "All planes in the system should live in the same dimension"
        case NoSolution = "No solutions"
        case InfiniteSolutions = "Infinitely many solutions"
    }

    private var planes: [MultiDimensionVector]
    private var dimensions: Int

    init(planes:[MultiDimensionVector]) {
        let planeDimensions = planes[0].dimensions
        for mdVector in planes {
            assert(mdVector.dimensions == planeDimensions, Message.AllPlanesMustBeInSameDimension.rawValue)
        }

        self.planes = planes
        self.dimensions = planeDimensions
    }

    func swapRows(row1 row1: Int, row2: Int) {
        let tempRow = planes[row2]
        planes[row2] = planes[row1]
        planes[row1] = tempRow
    }

    func multiplyRow(coefficient coefficient: Double, row: Int) -> Plane {
        let normalVector = planes[row].normalVector * coefficient
        let constant = planes[row].constantTerm * coefficient

        return Plane(x: normalVector.coordinate[0], y: normalVector.coordinate[1], z: normalVector.coordinate[2], constant: constant)
    }

    func multiplyCoefficientAndRow(coefficient coefficient: Double, row: Int) {
        planes[row] = multiplyRow(coefficient: coefficient, row: row)
    }

    func addMultipleTimesRowToRow(coefficient coefficient: Double, rowToMultiply: Int, addToRow: Int) {

        let multiple = multiplyRow(coefficient: coefficient, row: rowToMultiply)

        let rowToAddCoordinates = planes[addToRow].normalVector.coordinate
        let rowToAddConstant = planes[addToRow].constantTerm

        let multipliedCoordinates = multiple.normalVector.coordinate
        let multipliedConstant = multiple.constantTerm

        var newCoordinates = [Double]()
        for (index, value) in rowToAddCoordinates.enumerate() {
            newCoordinates.append(value + multipliedCoordinates[index])
        }

        let newConstant = rowToAddConstant + multipliedConstant

        planes[addToRow] = Plane(x: newCoordinates[0], y: newCoordinates[1], z: newCoordinates[2], constant: newConstant)
    }

    func indiciesOfFirstNonzeroItemsInEachRow() -> [Int] {
        let numberOfEquations = length()
        var indicies = [Int]()
        for _ in 1...numberOfEquations {
            indicies.append(-1)
        }

        for (index, value) in planes.enumerate() {
            assert(value.findFirstNonZeroIndex(value.normalVector) != nil, "No non-zero elements found!")
            indicies[index] = value.findFirstNonZeroIndex(value.normalVector)!
        }

        return indicies
    }

    func length() -> Int {
        return planes.count
    }

    func getItem(index: Int) -> MultiDimensionVector {
        return planes[index]
    }

    func setItem(index: Int, newItem: MultiDimensionVector) {
        assert(newItem.dimensions == dimensions, Message.AllPlanesMustBeInSameDimension.rawValue)
        planes[index] = newItem
    }

    func describe() -> String {
        var description = "Linear system:\n"
        for (index, value) in planes.enumerate() {
            description += "Equation \(index + 1): \(value.createStandardForm())\n"
        }
        return description
    }
}

extension Double {
    func isNearZero(eps: Double = 1e-10) -> Bool {
        return abs(self) < eps
    }
}

let p0 = Plane(x: 1, y: 1, z: 1, constant: 1)
let p1 = Plane(x: 0, y: 1, z: 0, constant: 2)
let p2 = Plane(x: 1, y: 1, z: -1, constant: 3)
let p3 = Plane(x: 1, y: 0, z: -2, constant: 2)

let lSystem1 = LinearSystem(planes: [p0, p1, p2, p3])

lSystem1.swapRows(row1: 0, row2: 1)
if !(lSystem1.planes[0] == p1 && lSystem1.planes[1] == p0) {
    print("test 1 failed")
}

lSystem1.swapRows(row1: 1, row2: 3)
lSystem1.swapRows(row1: 3, row2: 1)
if !(lSystem1.planes[0] == p1 && lSystem1.planes[1] == p0 && lSystem1.planes[2] == p2 && lSystem1.planes[3] == p3) {
    print("test 2 failed")
}

lSystem1.multiplyCoefficientAndRow(coefficient: 1, row: 0)
if !(lSystem1.planes[0] == p1 && lSystem1.planes[1] == p0 && lSystem1.planes[2] == p2 && lSystem1.planes[3] == p3) {
    print("test 4 failed")
}

lSystem1.multiplyCoefficientAndRow(coefficient: -1, row: 2)
if !(lSystem1.planes[0] == p1 &&
    lSystem1.planes[1] == p0 &&
    lSystem1.planes[2] == Plane(x: -1, y: -1, z: 1, constant: -3) &&
    lSystem1.planes[3] == p3) {
    print("test 5 failed")
}

lSystem1.multiplyCoefficientAndRow(coefficient: 10, row: 1)
if !(lSystem1.planes[0] == p1 &&
    lSystem1.planes[1] == Plane(x: 10, y: 10, z: 10, constant: 10) &&
    lSystem1.planes[2] == Plane(x: -1, y: -1, z: 1, constant: -3) &&
    lSystem1.planes[3] == p3) {
    print("test 6 failed")
}

lSystem1.addMultipleTimesRowToRow(coefficient: 0, rowToMultiply: 0, addToRow: 1)
if !(lSystem1.planes[0] == p1 &&
    lSystem1.planes[1] == Plane(x: 10, y: 10, z: 10, constant: 10) &&
    lSystem1.planes[2] == Plane(x: -1, y: -1, z: 1, constant: -3) &&
    lSystem1.planes[3] == p3) {
    print("test 7 failed")
}

lSystem1.addMultipleTimesRowToRow(coefficient: 1, rowToMultiply: 0, addToRow: 1)
if !(lSystem1.planes[0] == p1 &&
    lSystem1.planes[1] == Plane(x: 10, y: 11, z: 10, constant: 12) &&
    lSystem1.planes[2] == Plane(x: -1, y: -1, z: 1, constant: -3) &&
    lSystem1.planes[3] == p3) {
    print("test 8 failed")
}

lSystem1.addMultipleTimesRowToRow(coefficient: -1, rowToMultiply: 1, addToRow: 0)
if !(lSystem1.planes[0] == Plane(x: -10, y: -10, z: -10, constant: -10) &&
    lSystem1.planes[1] == Plane(x: 10, y: 11, z: 10, constant: 12) &&
    lSystem1.planes[2] == Plane(x: -1, y: -1, z: 1, constant: -3) &&
    lSystem1.planes[3] == p3) {
    print("test 9 failed")
}
