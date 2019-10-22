module DatabaseShelf where

import Control.Exception
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when, forM)
import Data.List(sort)
import Types
import Control.Category ((.), id)
import Data.Label
import Prelude hiding ((.), id)

currentBaseFilePath :: FilePath
currentBaseFilePath = "/Users/stephan/IdeaProjects/bookshelf-bot/Database/sql3"

initialize :: IConnection conn => conn -> IO ()
initialize conn = do
    tables <- getTables conn
    putStrLn "Initializing bd"
    when (not $ elem "books" tables) $ do
        run conn "create table books (\
                      \ bookId Integer not null primary key autoincrement,\
                      \ authorId Integer not null foreignKey, \
                      \ title Text, \
                      \ format Text,\
                      \ pathToFile Text, \
                      \ pathToCover Text, \
                      \ UIQUE(bookId, title) )"  []
        run conn "insert into table (bookId, authorId, title, format, pathToFile, pathToCover) \
                      \ values \
                      \ (?, ?, ?, ?, ?, ?) \
                      \ (?, ?, ?, ?, ?, ?) \
                      \ (?, ?, ?, ?, ?, ?)" [toSql (1 :: Int), toSql (1 :: Int), toSql ("aaa" :: String), toSql ("pdf" :: String), toSql ("" :: String), toSql ("" :: String), 
                                             toSql (2 :: Int), toSql (2 :: Int), toSql ("aab" :: String), toSql ("pdf" :: String), toSql ("" :: String), toSql ("" :: String),
                                             toSql (3 :: Int), toSql (2 :: Int), toSql ("aaa" :: String), toSql ("pdf" :: String), toSql ("" :: String), toSql ("" :: String)]                        
        >> return ()
    when (not $ elem "authors" tables) $ do
        run conn "create table authors  (\
                      \ authorId Integer not null primary key  autoincrement\
                      \ authorName Text)" []
        run conn "insert into table authors (authorId, authorName) \
                    \ values \ 
                    \ (?, ?) \
                    \ (?, ?)" [toSql (1 :: Int), toSql ("bbb" :: String),
                               toSql (2 :: Int), toSql ("bbc" :: String)]
        >> return ()


getBookByTitle :: IConnection conn => conn -> String -> IO (Either String Book)
getBookByTitle conn title = do
    res <- quickQuery' conn "select title authorId, format, pathToFile, pathToCover from BookShelf where title = ? " [toSql title]
    case res of
        [x] -> return $ Right $ convertBookFromSql x
        (x:xs) -> return (Left "There are many books with this title. Please specify the author")
        [] -> return (Left "There is no book with this title in my library")

getBookByAuthor :: IConnection conn => conn -> String -> String -> IO (Either String Book)
getBookByAuthor conn author title = do
    authorId <- quickQuery' conn "select authorId from authors where CONCAT (firstName, LastName) = ?" [toSql author]
    case authorId of
        [] -> return (Left "There is no such Author")
        _ -> do
            res <- quickQuery' conn "select title, authorId, format, pathToFile, pathToCover\
                            \ from BookShelf where title = ?, authorId = ?"
                            [toSql title, head $ head $ authorId]
            case res  of
                [] -> return (Left "This Author has no books with such name")
                [book] -> return (Right $ convertBookFromSql book)


getAuthor :: IConnection conn => conn -> Int -> IO (Either String Author)
getAuthor conn id = do
    maybeAuthor <- quickQuery' conn "select authorName from authors where Id = ?" [toSql id]
    case maybeAuthor of
        [] ->  return (Left "There is no such author")
        [[aName]]  -> return (Right Author {_Id = id, _name = fromSql aName})
        _  -> return (Left "Many Authors  with this Id. But how is it really can be possible?!?!?")


convertBookFromSql :: [SqlValue] -> Book
convertBookFromSql [title, aId, format, ptf, ptc]  = Book { _title = fromSql title
                                                          , _authorId = fromSql aId
                                                          , _format = [makeFormat $ fromSql format]
                                                          , _properties = (Properties (fromSql ptf) (fromSql ptc))
                                                          }

convertToRequestFormat :: Book -> Author -> (String, String)
convertToRequestFormat book author = (msg, fp)
    where
        fp = get (link . properties) book
        cp = get (coverLink . properties) book
        curTitle = get title book
        authorName = get name author
        msg = authorName ++ " - " ++ curTitle

insertAuthor :: IConnection conn => conn -> Author -> IO () 
insertAuthor conn author = do 
    run conn "insert into table authors (authorName) \
                \ values \ 
                \ (?) "
                 [toSql authName]    
    >> return ()
    where 
        authName = get name author
