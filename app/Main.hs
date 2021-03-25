module Main where

import Control.Monad (when)
import Control.Exception (try)
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, highlightCell, printFromBottom)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  loop initialRows Nothing []
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
    debugLinesCount = 20

    loop rows activeCellY debugMessages = do
      clearScreen
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      showInGrid
        xUpperLeft
        yUpperLeft
        columnCount
        columnWidth
        activeCellCoords
        (map (\row -> [smth row]) rows)
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
      printFromBottom
        xUpperLeft
        (yUpperLeft+12+debugLinesCount)
        debugMessages
      key <- getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
            let newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
                debugRow = "up, " ++ show(newActiveCellY)
                newDebugRows = take debugLinesCount (debugRow:debugMessages)
            loop rows newActiveCellY newDebugRows
          "\ESC[B" -> do -- down
            let newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
                debugRow = "down, " ++ show(newActiveCellY)
                newDebugRows = take debugLinesCount (debugRow:debugMessages)
            loop rows newActiveCellY newDebugRows
          "\n" -> do -- enter
            let eitherValue =
                  case activeCellY of
                    Nothing -> Left "there's no selected cell"
                    Just cellIndex -> do
                      if cellIndex < 0 || cellIndex >= (length rows)
                        then Left $ "index out of bounds: " ++ (show cellIndex)
                        else Right $ smth $ rows !! cellIndex
            let 

              showEditField value = do
                let
                  txt = "edit cell value:"
                  lentxt = length txt
                  yPos = 0
                  xPos = (columnCount * (columnWidth + 1)) + 3
                  replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                showInRectangle xPos yPos lentxt [txt, value]
                key <- getKey
                case key of
                  "\n" -> do
                    case activeCellY of
                      Nothing -> return ()
                      Just cellIndex -> do
                        clearRectangle xPos yPos lentxt 2
                        let newRows = replaceNth rows cellIndex (Row value)
                        loop newRows activeCellY debugMessages
                  "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                  c -> showEditField (value ++ c)
            case eitherValue of
              Left e -> do
                let msg = "error: " ++ (show e)
                loop rows activeCellY $ take debugLinesCount (msg:debugMessages)
              Right v -> do
                showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
