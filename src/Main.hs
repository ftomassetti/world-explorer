{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString.Lazy hiding (putStrLn, zip, foldr, take, repeat, filter, map)
import qualified PBWorld.World as PB
import qualified PBWorld.World.HeightMap as PBH
import qualified PBWorld.World.FloatRow as PBFR
import Text.ProtocolBuffers.WireMessage (messageGet)
import Graphics.UI.GLUT
import qualified Data.Map.Strict as M
import Data.Foldable (toList)

myPoints :: [(GLfloat,GLfloat,GLfloat)]
--myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
myPoints = [ (0.5, 0.5, 0.0)]

type MColor = (GLfloat,GLfloat,GLfloat)

toGlCoords :: Int -> Int -> IPoint -> (GLfloat,GLfloat,GLfloat)
toGlCoords width height (x,y) = let xf :: Float = fromIntegral x
                                    yf :: Float = fromIntegral y
                                    wf :: Float = fromIntegral width
                                    hf :: Float = fromIntegral height
                                    glX :: GLfloat = realToFrac $ ((xf / wf) * 2.0) - 1.0
                                    glY :: GLfloat = realToFrac $ ((yf / hf) * 2.0) - 1.0
                                in (glX, glY, 0.5)

toColoredPoints :: Int -> Int -> [IPoint] -> MColor -> [(MColor,(GLfloat,GLfloat,GLfloat))]
toColoredPoints width height coords color = let glCoords = map (toGlCoords width height) coords
                                            in zip (repeat color) glCoords

findPoints :: ElevationMap -> Int -> Int -> [(MColor,(GLfloat,GLfloat,GLfloat))]
findPoints elevMap width height = let values :: [(IPoint,Float)] = M.toList elevMap
                                      landValues  :: [(IPoint,Float)] = filter (\(_,e) -> e>0.5) values
                                      oceanValues :: [(IPoint,Float)] = filter (\(_,e) -> e<=0.5) values
                                      landCoords  :: [IPoint] = map fst landValues
                                      oceanCoords  :: [IPoint] = map fst oceanValues
                                      landColor :: MColor = (realToFrac 1.0, realToFrac 0.0, realToFrac 0.0)
                                      oceanColor :: MColor = (realToFrac 0.0, realToFrac 0.0, realToFrac 1.0)
                                  in toColoredPoints width height landCoords landColor ++ toColoredPoints width height oceanCoords oceanColor

type IPoint = (Int,Int)
type ElevationMap = M.Map IPoint Float

worldPath = "examples/seed_48675.world"

worldName = show . PB.name

worldWidth :: PB.World -> Int
worldWidth = fromIntegral . PB.width

worldHeight :: PB.World -> Int
worldHeight = fromIntegral . PB.height

worldHeightMap = PB.heightMap

worldElevationMap :: PB.World -> ElevationMap
worldElevationMap world = let heightMap = worldHeightMap world
                              rows = toList $ PBH.rows heightMap
                              rowsWithIndex = zip [0..] rows
                          in foldr addRow M.empty rowsWithIndex
                          where width = worldWidth world
                                addRow :: (Int,PBFR.FloatRow) -> ElevationMap -> ElevationMap
                                addRow (y,row) map = let cells = toList $ PBFR.cells row
                                                         coords = take width (zip (repeat y) [0..])
                                                         cellsWithCoords = zip coords cells
                                                     in foldr addCell map cellsWithCoords
                                addCell :: (IPoint,Float) -> ElevationMap -> ElevationMap
                                addCell (point,elev) map = M.insert point elev map

loadWorld :: FilePath -> IO PB.World
loadWorld filename = do
    input <- Data.ByteString.Lazy.readFile filename
    world <- case messageGet input of
        Right (world,_) -> return world :: IO PB.World
        Left msg -> error $ "Fail :( " ++ msg
    putStrLn "Loaded"
    putStrLn $ " name: " ++ worldName world
    putStrLn $ " width: " ++ show (worldWidth world)
    return world

color3f r g b = color $ Color3 r g (b :: GLfloat)

drawColoredPoint :: (MColor,(GLfloat,GLfloat,GLfloat)) -> IO ()
drawColoredPoint ((r,g,b), (x,y,z)) = do color3f r g b
                                         vertex $ Vertex3 x y z

display :: ElevationMap -> Int -> Int -> DisplayCallback
display elevMap width height = do
  clear [ ColorBuffer ]
  let points = findPoints elevMap width height
  renderPrimitive Points $
       mapM_ drawColoredPoint points
  flush

main = do
    world <- loadWorld worldPath
    let elevMap = worldElevationMap world
        width   = worldWidth world
        height  = worldHeight world
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "World Explorer"
    displayCallback $= display elevMap width height
    mainLoop


