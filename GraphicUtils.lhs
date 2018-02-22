> module GraphicUtils where

> import Graphics.UI.WX
> import ModelType

> {-type LastShapePos 	= ItemState -- Start and End Position of last shape
> type CurrentShpPos 	= ItemState
> szZero = sz 0 0
>
> szW (Size w h) = w
> szH (Size w h) = h

> getSize :: Point -> Point -> Size -> Size -- 3rd arg size is add extra in size
> getSize (Point x y) (Point x1 y1) (Size w h) = sz (x1-x+w) (y1-y+h)

> szWidth (Size w h) = w
> szHeight (Size w h) = h

> addToPoint :: Point -> Size -> Point
> addToPoint (Point x y) (Size w h) = Point (x+w) (y+h)

> addToSize :: Size -> Size -> Size
> addToSize (Size w h) (Size w1 h1) 	= (sz (w+w1) (h+h1))

> addToPosition :: (Point, Point) -> (Size, Size) -> (Point, Point) 
> addToPosition (p1, p2) (s1, s2) 	= (,) (addToPoint p1 s1) (addToPoint p2 s2)

> topLeft, topRight, bottomLeft, bottomRight :: (Point, Point) -> Point
> topLeft (p1, p2) = p1 
> topRight (p1, p2) = pt (pointX p2) (pointY p1)
> bottomLeft (p1, p2) = pt (pointX p1) (pointY p2)
> bottomRight (p1, p2) = p2-}

> plusRect :: Int -> Rect -> Rect
> plusRect u (Rect l t w h) = Rect (l-u) (t-u) (w+u*2) (h+u*2)

> insideButtonArea :: Point -> VirtualButton -> Bool
> insideButtonArea p b = insideRectArea p (btnPos b) (btnSz b)

> insideGIArea :: Point -> ItemState -> Bool
> insideGIArea p vi = 
>	insideRectArea p (itemPosition vi) (itemSize vi)

> insideGIOutputArea :: Point -> ItemState -> Bool
> insideGIOutputArea p vi = 
>	let 	Point x y	= itemPosition vi
>		Size w h	= itemSize vi
>		wHalf		= w `div` 2
>		hHalf		= h `div` 2
>	in	insideRectArea p (pt (x) (y + hHalf)) (sz w hHalf)

> insideRectArea :: Point -> Point -> Size -> Bool
> insideRectArea (Point x y) (Point x1 y1) (Size w h) = 
>	(x >= x1) && (x <= (x1 + w)) && (y >= y1) && (y <= (y1 + h))

> addToPoint :: Point -> Size -> Point
> addToPoint (Point x y) (Size w h) = Point (x+w) (y+h)

> triHead :: DC a -> ItemState -> Int -> [Prop (DC a)] -> IO ()
> triHead dc vi s prop =
>	do
>	let Point x y = itemPosition vi
>	let Size w h = itemSize vi
>	let p1 = Point (x + w) y
>	let p2 = Point (x + w + s) (y + h `div` 2)
>	let p3 = Point (x + w) (y + h)
>	polygon dc [p1, p2, p3] prop

> triHeadLeft :: DC a -> ItemState -> Int -> [Prop (DC a)] -> IO ()
> triHeadLeft dc vi s prop =
>	do
>	let Point x y = itemPosition vi
>	let Size w h = itemSize vi
>	let p1 = Point x y
>	let p2 = Point (x - s) (y + h `div` 2)
>	let p3 = Point x (y + h)
>	polygon dc [p1, p2, p3] prop

> triArrow :: DC a -> Point -> [Prop (DC a)] -> IO ()
> triArrow dc pt1 prop =
>	do
>	let Point x y = pt1
>	let p1 = Point x y
>	let p2 = Point (x - 4) (y - 4)
>	let p3 = Point (x + 4) (y - 4)
>	polygon dc [p1, p2, p3] prop

