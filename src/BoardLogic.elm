module BoardLogic exposing (BoardRect, Position, distance, movedBeyond, nextClampedPosition)


type alias Position =
    { x : Float
    , y : Float
    }


type alias BoardRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


distance : Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =
    sqrt (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))


movedBeyond : Float -> Float -> Float -> Float -> Float -> Bool
movedBeyond threshold startX startY currentX currentY =
    distance startX startY currentX currentY > threshold


nextClampedPosition :
    Float
    -> Float
    -> Float
    -> BoardRect
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Position
nextClampedPosition miniatureWidth miniatureHeight miniScale rect startMouseX startMouseY startCardX startCardY clientX clientY =
    let
        safeWidth =
            if rect.width <= 0 then
                1

            else
                rect.width

        safeHeight =
            if rect.height <= 0 then
                1

            else
                rect.height

        deltaX =
            (clientX - startMouseX) / safeWidth

        deltaY =
            (clientY - startMouseY) / safeHeight

        marginX =
            ((miniatureWidth * miniScale) / 2) / safeWidth

        marginY =
            ((miniatureHeight * miniScale) / 2) / safeHeight
    in
    { x = clamp marginX (1 - marginX) (startCardX + deltaX)
    , y = clamp marginY (1 - marginY) (startCardY + deltaY)
    }


clamp : Float -> Float -> Float -> Float
clamp minVal maxVal value =
    if value < minVal then
        minVal

    else if value > maxVal then
        maxVal

    else
        value
