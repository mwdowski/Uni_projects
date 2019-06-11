library("sqldf")
library("dplyr")
library("data.table")

#1:
df_sql_1 <- function(Users, Posts){
    #Pokazuje dane uzytkownikow, ktorych posty typu "question" zdobyly najwiecej polubien. Oprocz tego pokazuje ich najbardziej lubiane pytanie
    sqldf ("SELECT
                Users.DisplayName,
                Users.Age,
                Users.Location,
                SUM(Posts.FavoriteCount) AS FavoriteTotal,
                Posts.Title AS MostFavoriteQuestion,
                MAX(Posts.FavoriteCount) AS MostFavoriteQuestionLikes
            FROM Posts JOIN Users ON Users.Id=Posts.OwnerUserId
            WHERE Posts.PostTypeId=1
            GROUP BY OwnerUserId
            ORDER BY FavoriteTotal DESC
            LIMIT 10") -> final
    return(final)
} #example

df_base_1 <- function(Users, Posts){
    #Wydobycie potrzebnych danych z ramki danych Users
    Users <- Users[, c("DisplayName", "Age", "Location", "Id")]
    
    #Wydobycie potrzebnych danych z ramki danych Posts i obrobka - zamiana NA na 0 i wziecie jedynie pytan
    Posts$FavoriteCount[is.na(Posts$FavoriteCount)] <- 0
    Posts <- Posts[, c("OwnerUserId", "FavoriteCount", "PostTypeId", "Title")]
    Posts <- Posts[Posts$PostTypeId==1, ]
    
    #Policzenie sumy polubien dla kazdego uzytkownika i posortowanie ich malejaco
    df1 <- aggregate(x=Posts$FavoriteCount, by=Posts["OwnerUserId"], sum)
    rightOrder <- order(-df1$x)
    df1 <- df1[rightOrder, ]
    
    #Policzenie najbardziej lubianego postu dla kazdego uzytkownika i posortowanie go wg kolejnosci uzytkownikow z ramki df1
    df2 <- do.call(rbind, lapply(split(Posts, Posts$OwnerUserId), function(chunk) chunk[which.max(chunk$FavoriteCount),]))
    df2 <- df2[rightOrder, ]
    
    #Polaczenie danych
    final <- cbind(df1, df2)
    
    #Znalezienie danych uzytkonikow po unikatowym kluczu OwnerUserId
    #Uzylem funkcji match, bo wtedy nie znalem jeszcze merge. Mysle, ze dla celow edukacyjnych (i z wrodzonego lenistwa) zachowam to rozwiazanie
    Users <- Users[match(final$OwnerUserId, Users$Id), ]
    final <- cbind(final, Users)
    
    #Wybranie kolumn, zmiana nazw, dopasowanie typow
    final <- final[, c("DisplayName", "Age", "Location", "x", "Title", "FavoriteCount")]
    colnames(final) <- c("DisplayName", "Age", "Location", "FavoriteTotal", "MostFavoriteQuestion", "MostFavoriteQuestionLikes")
    return(head(final, n=10))
} #done

df_dplyr_1 <- function(Users, Posts){
    Posts %>% dplyr::do(replace(., is.na(.), 0)) -> Posts
    Posts %>%
        dplyr::filter(PostTypeId==1) %>%
        dplyr::select(OwnerUserId, FavoriteCount) %>%
        dplyr::group_by(OwnerUserId) %>%
        dplyr::summarise(FavoriteTotal=sum(FavoriteCount)) %>%
        dplyr::arrange(desc(FavoriteTotal)) -> Posts1
    Posts %>%
        dplyr::filter(PostTypeId==1) %>%
        dplyr::select(OwnerUserId, FavoriteCount, Title) %>%
        dplyr::group_by(OwnerUserId, Title) %>%
        dplyr::summarise(MostFavoriteQuestionLikes=max(FavoriteCount)) %>%
        dplyr::filter(MostFavoriteQuestionLikes==max(MostFavoriteQuestionLikes)) %>%
        dplyr::inner_join(Posts1, by=c("OwnerUserId" = "OwnerUserId")) -> Posts2
    Users %>%
        dplyr::select(DisplayName, Age, Location, Id) %>%
        dplyr::inner_join(Posts2, by=c("Id" = "OwnerUserId")) %>%
        dplyr::arrange(desc(FavoriteTotal)) %>%
        dplyr::select(DisplayName, Age, Location, FavoriteTotal, MostFavoriteQuestion=Title, MostFavoriteQuestionLikes) %>%
        dplyr::slice(1:10)-> Users
    return(Users)
} #done

df_table_1 <- function(Users, Posts){
    Posts[is.na(Posts)] <- 0
    Posts1 <- Posts[PostTypeId==1, .(OwnerUserId, FavoriteCount)] [, .(FavoriteTotal=sum(FavoriteCount)), by=.(OwnerUserId)]
    Posts2 <- Posts[PostTypeId==1, .(OwnerUserId, FavoriteCount, Title)] [, .(MostFavoriteQuestionLikes=max(FavoriteCount)), by=.(OwnerUserId, Title)] [, .SD[which.max(MostFavoriteQuestionLikes)], by=.(OwnerUserId)]
    setkey(Posts1, OwnerUserId)
    setkey(Posts2, OwnerUserId)
    Posts1 <- Posts1[Posts2, nomatch=0]
    setkey(Users, Id)
    Users <- Users[, .(DisplayName, Age, Location, Id)] [Posts1, nomatch=0] [order(-FavoriteTotal), .(DisplayName, Age, Location, FavoriteTotal, MostFavoriteQuestion=Title, MostFavoriteQuestionLikes)] [1:10]
    return(Users)
} #done

#2:
df_sql_2 <- function(Posts){
    #Pokazuje ID i tytul postow, ktore maja najwiecej pozytywnie ocenianych odpowiedzi
    sqldf ("SELECT
                Posts.ID,
                Posts.Title,
                Posts2.PositiveAnswerCount
            FROM Posts
            JOIN (
                SELECT
                    Posts.ParentID,
                    COUNT(*) AS PositiveAnswerCount
                FROM Posts
                WHERE Posts.PostTypeID=2 AND Posts.Score>0
                GROUP BY Posts.ParentID
            ) AS Posts2
            ON Posts.ID=Posts2.ParentID
            ORDER BY Posts2.PositiveAnswerCount DESC
            LIMIT 10") -> final
    return(final)
} #example

df_base_2 <- function(Posts){
    #Filtrowanie zeby wydobyc pozytywnie ocenione odpowiedzi
    Posts2 <- Posts[Posts$PostTypeId==2 & Posts$Score>0, ]
    
    #Zliczenie pozytynie ocenionych odpowiedzi dla kazdego postu i zmiana nazwy kolumny
    Posts2 <- aggregate(x=Posts2$ParentId, by=Posts2["ParentId"], FUN=length)
    colnames(Posts2)[2] <- "PositiveAnswerCount"
    
    #Wybranie kolumn i polaczenie z Posts2 po Id postu
    Posts <- Posts[, c("Id", "Title")]
    Posts <- merge(x=Posts, y=Posts2, by.x="Id", by.y="ParentId")
    
    #Sortowanie malejaco
    Posts <- Posts[order(-Posts2$PositiveAnswerCount), ]
    return(head(Posts, n=10))
} #done

df_dplyr_2 <- function(Posts){
    Posts %>%
        dplyr::filter(PostTypeId==2, Score>0) %>%
        dplyr::group_by(ParentId) %>%
        dplyr::summarise(PositiveAnswerCount=n()) -> Posts2
    Posts %>%
        dplyr::inner_join(Posts2, by=c("Id" = "ParentId")) %>%
        dplyr::select(Id, Title, PositiveAnswerCount) %>%
        dplyr::arrange(desc(PositiveAnswerCount)) %>%
        dplyr::slice(1:10) -> Posts
    return(Posts)
} #done

df_table_2 <- function(Posts){
    Posts2 <- Posts[PostTypeId==2 & Score>0, .(PositiveAnswerCount=.N), by=.(ParentId)]
    setkey(Posts, Id)
    setkey(Posts2, ParentId)
    Posts <- Posts[Posts2, nomatch=0] [order(-PositiveAnswerCount), c("Id", "Title", "PositiveAnswerCount")] [1:10]
    return(Posts)
} #done

#3:
df_sql_3 <- function(Posts, Votes){
    #dla kazdego roku pokazuje pytanie, ktore mialo najwiecej glosow typu 2 (wraz z wynikiem)
    sqldf ("SELECT
                Posts.Title,
                UpVotesPerYear.Year,
                MAX(UpVotesPerYear.Count) AS Count
            FROM (
                SELECT
                    PostId,
                    COUNT(*) AS Count,
                    STRFTIME('%Y', Votes.CreationDate) AS Year
                FROM Votes
                WHERE VoteTypeId=2
                GROUP BY PostId, Year
            ) AS UpVotesPerYear
            JOIN Posts ON Posts.Id=UpVotesPerYear.PostId
            WHERE Posts.PostTypeId=1
            GROUP BY Year") -> final
    return(final)
} #example

df_base_3 <- function(Posts, Votes){
    #Tworzenie zestawienia glosow dla kazdego postu w zaleznosci od roku
    UpVotesPerYear <- Votes[Votes$VoteTypeId==2, ]
    UpVotesPerYear[, "CreationDate"] <- substr(UpVotesPerYear[, "CreationDate"], 1, 4)
    UpVotesPerYear <- aggregate(UpVotesPerYear$Id, by=list(UpVotesPerYear$PostId , UpVotesPerYear$CreationDate), FUN=length)
    colnames(UpVotesPerYear) <- c("PostId", "Year", "Count")
    
    #Polaczenie glosow z postami i wybranie maksymalnego wyniku dla kazdego roku
    Posts <- Posts[Posts$PostTypeId==1, ]
    Posts <- Posts[, c("Id", "Title")]
    Posts <- merge(x=Posts, y=UpVotesPerYear, by.x="Id", by.y="PostId")
    Posts <- do.call(rbind, lapply(split(Posts, Posts$Year), function(chunk) chunk[which.max(chunk$Count),]))
    Posts <- Posts[, c("Title", "Year", "Count")]
    return(Posts)
} #done

df_dplyr_3 <- function(Posts, Votes){
    Votes %>%
        dplyr::filter(VoteTypeId==2) %>%
        dplyr::mutate(Year=substr(CreationDate, 1, 4)) %>%
        dplyr::group_by(PostId, Year) %>%
        dplyr::summarise(Count=n()) %>%
        dplyr::select(PostId, Year, Count) -> UpVotesPerYear
    Posts %>%
        dplyr::filter(PostTypeId==1) %>%
        dplyr::select(Id, Title) %>%
        dplyr::inner_join(UpVotesPerYear, by=c("Id" = "PostId")) %>%
        dplyr::group_by(Year) %>%
        dplyr::filter(Count==max(Count)) %>%
        dplyr::select(-Id) -> Posts
    return(Posts)
} #done

df_table_3 <- function(Posts, Votes){
    UpVotesPerYear <- Votes[VoteTypeId==2, .(PostId, Year=substr(CreationDate, 1, 4))] [, .(Count=.N), by=.(PostId, Year)]
    setkey(Posts, Id)
    setkey(UpVotesPerYear, PostId)
    Posts <- Posts[PostTypeId==1, .(Id, Title)] [UpVotesPerYear, nomatch=0] [, .SD[which.max(Count)], by=.(Year)] [, .(Title, Year, Count)]
    return(Posts)
} #done

#4:
df_sql_4 <- function(Posts){
    #Pokazuje ID i tytul pytan, w ktorych roznica miedzy ocena zaakceptowanej odpowiedzi, a ocena najlepszej odpowiedzi, przekracza 50 (posortowane malejaco wg tej roznicy)
    sqldf ("SELECT
                Questions.Id,
                Questions.Title,
                BestAnswers.MaxScore,
                Posts.Score AS AcceptedScore,
                BestAnswers.MaxScore-Posts.Score AS Difference
            FROM (
                    SELECT Id, ParentId, MAX(Score) AS MaxScore
                    FROM Posts
                    WHERE PostTypeId==2
                    GROUP BY ParentId
                ) AS BestAnswers
            JOIN (
                    SELECT * FROM Posts
                    WHERE PostTypeId==1
                ) AS Questions
                ON Questions.Id=BestAnswers.ParentId
            JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
            WHERE Difference>50
            ORDER BY Difference DESC") -> final
    return(final)
} #example

df_base_4 <- function(Posts){
    #Wydobycie potrzebnych danych do stworzenia tabeli BestAnswers
    BestAnswers <- Posts[, c("Id", "ParentId", "PostTypeId","Score")]
    
    #Przefiltrowanie tak, by w tabeli byly tylko odpowiedzi
    BestAnswers <- BestAnswers[BestAnswers$PostTypeId==2, ]
    
    #Znalezienie oceny najwyzej ocenianej odpowiedzi dla kazdego zapytania
    BestAnswers <- aggregate(x=BestAnswers$Score, by=BestAnswers["ParentId"], max)
    
    #Zmiana nazwy kolumny z wynikami dla kazdego ParentId
    colnames(BestAnswers)[2] <- "MaxScore"
    
    #Przefiltrowanie (aby zostaly tylko pytania), zmiana nazwykolumny i wydobycie potrzebnych danych
    Questions <- Posts[Posts$PostTypeId==1, ]
    colnames(Questions)[8] <- "MainId"
    Questions <- Questions[, c("MainId", "Title", "AcceptedAnswerId")]
    
    #Polaczenie pytan z najlepszymi pytaniami i z postami
    Questions <- merge(x=Questions, y=BestAnswers, by.x="MainId", by.y="ParentId")
    Questions <- merge(x=Posts, y=Questions, by.x="Id", by.y="AcceptedAnswerId")
    
    #Stworzenie kolumny z roznica miedzy wynikiem najlepiej ocenianej odpwoiedzi a zaakceptowanej odpowiedzi
    Difference <- Questions$MaxScore - Questions$Score
    Questions <- cbind(Questions, Difference)
    
    #Zmiana nazw i kolejnosci kolumn
    Questions <- Questions[, c("MainId", "Title.y", "MaxScore", "Score", "Difference")]
    colnames(Questions) <- c("Id", "Title", "MaxScore", "AcceptedScore", "Difference")
    
    #Przefiltrowanie i sortowanie rekordow tak, zeby pokazane byly roznice wieksze niz 50, w porzadku malejacym
    Questions <- Questions[Questions$Difference>50, ]
    Questions <- Questions[order(-Questions$Difference), ]
    return(Questions)
} #done

df_dplyr_4 <- function(Posts){
    Posts %>%
        dplyr::select(Id, ParentId, PostTypeId, Score) %>%
        dplyr::filter(PostTypeId==2) %>%
        dplyr::group_by(ParentId) %>%
        dplyr::summarise(MaxScore=max(Score)) -> BestAnswers
    Posts %>%
        dplyr::filter(PostTypeId==1) %>%
        dplyr::select(MainId=Id, Title, AcceptedAnswerId) %>%
        dplyr::inner_join(BestAnswers, by=c("MainId" = "ParentId")) %>%
        dplyr::inner_join(Posts, by=c("AcceptedAnswerId" = "Id")) %>%
        dplyr::mutate(Difference = MaxScore - Score) %>%
        dplyr::select(Id=MainId, Title=Title.x, MaxScore, AcceptedScore=Score, Difference) %>%
        dplyr::filter(Difference>50) %>%
        dplyr::arrange(desc(Difference)) -> Questions
    return(Questions)
} #done

df_table_4 <- function(Posts){
    BestAnswers <- Posts[PostTypeId==2, c("Id", "ParentId" , "Score")] [, .(MaxScore=max(Score)), by=.(ParentId)]
    Questions <- Posts[PostTypeId==1, .(MainId=Id, Title, AcceptedAnswerId)]
    
    setkey(Questions, MainId)
    setkey(BestAnswers, ParentId)
    Questions <- Questions[BestAnswers]
    
    setkey(Questions, AcceptedAnswerId)
    setkey(Posts, Id)
    Questions <- Questions[Posts] [, .(Id=MainId, Title, MaxScore, AcceptedScore=Score, Difference=(MaxScore-Score) )] [Difference>50] [order(-Difference)]
    return(Questions)
} #done

#5:
df_sql_5 <- function(Posts, Comments){
    #Pokazuje tytuly postow, ktorych autor zdobyl najwieksza sume ocen komentarzy pod nim
    sqldf ("SELECT
            Posts.Title,
            CmtTotScr.CommentsTotalScore
            FROM (
                SELECT
                PostID,
                UserID,
                SUM(Score) AS CommentsTotalScore
                FROM Comments
                GROUP BY PostID, UserID
            ) AS CmtTotScr
            JOIN Posts ON Posts.ID=CmtTotScr.PostID AND Posts.OwnerUserId=CmtTotScr.UserID
            WHERE Posts.PostTypeId=1
            ORDER BY CmtTotScr.CommentsTotalScore DESC
            LIMIT 10") -> final
    return(final)
} #example

df_base_5 <- function(Posts, Comments){
    #Wybranie potrzebnych kolumn z Comments
    CmtTotScr <- Comments[, c("PostId", "UserId", "Score")]
    
    #Zliczenie sumy ocen komentarzy dla kazdego posta i uzytkownika, potem zmiana nazwy kolumny
    CmtTotScr <- aggregate(CmtTotScr$Score, list(PostId=CmtTotScr$PostId, UserId=CmtTotScr$UserId), sum)
    colnames(CmtTotScr)[3] <- "CommentsTotalScore"
    
    #Wybranie tylko postow typu 1 oraz wybranie potrzebnych wierszy
    Posts <- Posts[Posts$PostTypeId==1, ]
    Posts <- Posts[, c("Id", "OwnerUserId", "Title")]
    
    #Polaczenie Posts i CmtTotScr po uzytkowniku, ktory jest wlascicielem posta, oraz po id posta
    Posts <- merge(x=Posts, y=CmtTotScr, by.x=c("OwnerUserId", "Id"), by.y=c("UserId", "PostId"))
    
    #Wybranie potrzebnych kolumn i sortowanie
    Posts <- Posts[, c("Title", "CommentsTotalScore")]
    Posts <- Posts[order(-Posts$CommentsTotalScore), ]
    return(head(Posts, n=10))
} #done

df_dplyr_5 <- function(Posts, Comments){
    Comments %>%
        dplyr::select(PostId, UserId, Score) %>%
        dplyr::group_by(PostId, UserId) %>%
        dplyr::summarise(CommentsTotalScore=sum(Score)) -> CmtTotScr
    Posts %>%
        dplyr::filter(PostTypeId==1) %>%
        dplyr::select(Id, OwnerUserId, Title) %>%
        dplyr::inner_join(CmtTotScr, by=c("Id" = "PostId", "OwnerUserId" = "UserId")) %>%
        dplyr::select(Title, CommentsTotalScore) %>%
        dplyr::arrange(desc(CommentsTotalScore)) %>%
        dplyr::slice(1:10) -> Posts
    return(Posts)
} #done

df_table_5 <- function(Posts, Comments){
    CmtTotScr <- Comments[, .(CommentsTotalScore=sum(Score)), by=.(PostId, UserId)]
    setkey(Posts, Id, OwnerUserId)
    setkey(CmtTotScr, PostId, UserId)
    Posts <- Posts[PostTypeId==1, c("Id", "OwnerUserId", "Title")] [CmtTotScr, nomatch=0] [order(-CommentsTotalScore), c("Title", "CommentsTotalScore")] [1:10]
    return(Posts)
} #done

#6:
df_sql_6 <- function(Users, Badges){
    #Pokazuje dane uzytkownikow, ktorzy zdobyli medale klasy 1, ktore zostaly zdobyte od 2 do 10 razy
    sqldf ("SELECT DISTINCT
                Users.Id,
                Users.DisplayName,
                Users.Reputation,
                Users.Age,
                Users.Location
            FROM (
                    SELECT
                        Name, UserID
                    FROM Badges
                    WHERE Name IN (
                        SELECT
                            Name
                        FROM Badges
                        WHERE Class=1
                        GROUP BY Name
                        HAVING COUNT(*) BETWEEN 2 AND 10
                    )
                    AND Class=1
                ) AS ValuableBadges
            JOIN Users ON ValuableBadges.UserId=Users.Id") -> final
    return(final)
} #example

df_base_6 <- function(Users, Badges){
    #Wydobycie tylko medale klasy 1
    FirstClassBadges <- Badges[Badges$Class==1, ]
    
    #Tworzenie zestawienia zdobyc medalu w zaleznowsi od nazwy, potem ustawienie nazw kolumn
    FirstClassBadges <- as.data.frame(table(FirstClassBadges$Name), stringsAsFactors=FALSE)
    colnames(FirstClassBadges) <- c("Name", "Freq")
    
    #Wydobycie tylko medali zdobytych 2-10 razy
    FirstClassBadges <- FirstClassBadges[FirstClassBadges$Freq >= 2 & FirstClassBadges$Freq <= 10, ]
    FirstClassBadges <- FirstClassBadges[, "Name"]
    
    #Wydobycie danych o zdobytych medalach klasy 1
    ValuableBadges <- Badges[, c("Name", "UserId", "Class")]
    ValuableBadges <- ValuableBadges[ValuableBadges$Class==1, ]
    
    #Znalezienie medali, ktore sa zdefiniowane w FirstClassBadges
    ValuableBadges <- ValuableBadges[ValuableBadges$Name %in% FirstClassBadges, ]
    
    #Wydobycie danych o uzytkownikach
    Users <- Users[, c("Id", "DisplayName", "Reputation", "Age", "Location")]
    
    #Polaczenie uzytkownikow z medalami z ValuableBadges i pokazanie unikatowych rekordow
    Users <- merge(x=Users, y=ValuableBadges, by.x="Id", by.y="UserId")
    Users <- unique(Users[, c("Id", "DisplayName", "Reputation", "Age", "Location")])
    return(Users)
} #done

df_dplyr_6 <- function(Users, Badges){
    Badges %>%
        dplyr::filter(Class==1) %>%
        dplyr::group_by(Name) %>%
        dplyr::summarise(n=n()) %>%
        dplyr::filter(n>=2 & n<=10) -> FCB
    Badges %>%
        dplyr::select(Name, UserId, Class) %>%
        dplyr::filter(Class==1 & Name %in% FCB$Name) -> VB
    Users %>%
        dplyr::inner_join(VB, by=c("Id" = "UserId")) %>%
        dplyr::distinct(Id, .keep_all=TRUE) %>%
        dplyr::select(Id, DisplayName, Reputation, Age, Location) -> Users
    return(Users)
} #done

df_table_6 <- function(Users, Badges){
    FCB <- Badges[Class==1] [, .(n=.N), by=.(Name)] [n>=2 & n<=10]
    VB <- Badges[Class==1 & Name %in% FCB[, Name], .(Name, UserId, Class)]
    
    setkey(Users, Id)
    setkey(VB, UserId)
    Users <- Users[VB] [, .(Id, DisplayName, Reputation, Age, Location), by=.(Id, DisplayName, Reputation, Age, Location)]
    return(Users)
} #done

#7:
df_sql_7 <- function(Posts, Votes){
    #Pokazuje tytuły postów typu 1, które mają najwięcej "starych" głosów typu "2", i nie mają "nowych" głosów
    sqldf ("SELECT
                Posts.Title,
                VotesByAge2.OldVotes
            FROM Posts
            JOIN (
                SELECT
                    PostId,
                    MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
                    MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
                    SUM(Total) AS Votes
                FROM (
                    SELECT
                        PostId,
                        CASE STRFTIME('%Y', CreationDate)
                            WHEN '2017' THEN 'new'
                            WHEN '2016' THEN 'new'
                            ELSE 'old'
                            END VoteDate,
                        COUNT(*) AS Total
                    FROM Votes
                    WHERE VoteTypeId=2
                    GROUP BY PostId, VoteDate
                ) AS VotesByAge
                GROUP BY VotesByAge.PostId
                HAVING NewVotes=0
            ) AS VotesByAge2 ON VotesByAge2.PostId=Posts.ID
            WHERE Posts.PostTypeId=1
            ORDER BY VotesByAge2.OldVotes DESC
            LIMIT 10") -> final
    return(final)
} #example

age <- function(x){
    #funkcja pomocnicza do zadania 7 - zwraca informacje, czy cos jest "stare" czy "nowe". Ze względu na to, że lata są z zakresu 2011-2017, wystarczy porownanie leksykograficzne
    x[x>=2016] <- "new"
    x[x<2016] <- "old"
    return(x)
}

df_base_7 <- function(Posts, Votes){
    #Wybranie glosow typu 2
    VotesByAge <- Votes[Votes$VoteTypeId==2, ]
    #Obrobka daty i posegregowanie na stare i nowe
    VotesByAge["CreationDate"] <- age(substr(VotesByAge$CreationDate, 1, 4))
    colnames(VotesByAge)[2] <- "VoteDate"
    #Liczenie liczby starych i nowych glosow dla kazdego posta
    VotesByAge <- aggregate(x=VotesByAge$Id, by=list(VotesByAge$PostId, VotesByAge$VoteDate), FUN=length)
    colnames(VotesByAge) <- c("PostId", "VoteDate", "Total")
    
    #Utworzenie dwoch tabel - na nowe i na stare glosy
    VotesByAgeNew <- VotesByAge[VotesByAge$VoteDate=="new", ]
    VotesByAgeOld <- VotesByAge[VotesByAge$VoteDate=="old", ]
    
    #Liczenie max dla kazdego postu wsrod starych i nowych glosow
    VotesByAgeNew <- aggregate(x=VotesByAgeNew$Total, by=VotesByAgeNew["PostId"], FUN=max)
    VotesByAgeOld <- aggregate(x=VotesByAgeOld$Total, by=VotesByAgeOld["PostId"], FUN=max)
    
    #Polaczenie tablic ale z pokazaniem niepasujacych wartosci, tam bedzie zero zamiast NA
    VotesByAge2 <- merge(x=VotesByAgeNew, y=VotesByAgeOld, by.x="PostId", by.y="PostId", all=TRUE)
    colnames(VotesByAge2) <- c("PostId", "NewVotes", "OldVotes")
    VotesByAge2$NewVotes[is.na(VotesByAge2$NewVotes)] <- 0
    VotesByAge2$OldVotes[is.na(VotesByAge2$OldVotes)] <- 0
    
    #Segregacja, tak zeby byly posty, ktore mają tylko stare głosy
    VotesByAge2 <- VotesByAge2[VotesByAge2$NewVotes==0, ]
    
    #Polaczenie z postami typu 1, segregacja, sortowanie
    Posts <- Posts[Posts$PostTypeId==1, ]
    Posts <- Posts[, c("Title", "Id")]
    Posts <- merge(x=Posts, y=VotesByAge2, by.x="Id", by.y="PostId")
    Posts <- Posts[, c("Title", "OldVotes")]
    Posts <- Posts[order(-Posts$OldVotes), ]
    return(head(Posts, n=10))
} #done

df_dplyr_7 <- function(Posts, Votes){
    Votes %>%
        dplyr::filter(VoteTypeId==2) %>%
        dplyr::mutate(VoteDate=age(CreationDate)) %>%
        dplyr::group_by(PostId, VoteDate) %>%
        dplyr::summarise(Total=n()) -> VotesByAge
    VotesByAge %>%
        dplyr::filter(VoteDate=="new") %>%
        dplyr::group_by(PostId) %>%
        dplyr::summarise(NewVotes=max(Total)) -> VotesByAgeNew
    VotesByAge %>%
        dplyr::filter(VoteDate=="old") %>%
        dplyr::group_by(PostId) %>%
        dplyr::summarise(OldVotes=max(Total)) -> VotesByAgeOld
    Posts %>%
        dplyr::filter(PostTypeId==1) -> Posts
    VotesByAgeOld %>%
        dplyr::full_join(VotesByAgeNew, by=c("PostId" = "PostId")) %>%
        dplyr::filter(is.na(NewVotes)) %>%
        dplyr::inner_join(Posts, by=c("PostId" = "Id")) %>%
        dplyr::select(Title, OldVotes) %>%
        dplyr::arrange(desc(OldVotes)) %>%
        dplyr::slice(1:10) -> Final
    return(Final)
} #done

df_table_7 <- function(Posts, Votes){
    VotesByAge <- Votes[VoteTypeId==2, .(Id, PostId, VoteDate=age(CreationDate))] [, .(Total=.N), by=.(PostId, VoteDate)]
    VotesByAgeNew <- VotesByAge[VoteDate=="new", .(NewVotes=max(Total)), by=.(PostId)]
    VotesByAgeOld <- VotesByAge[VoteDate=="old", .(OldVotes=max(Total)), by=.(PostId)]
    
    Posts <- Posts[PostTypeId==1]
    
    setkey(Posts, Id)
    setkey(VotesByAgeNew, PostId)
    setkey(VotesByAgeOld, PostId)
    Final <- VotesByAgeNew[VotesByAgeOld] [is.na(NewVotes)] [Posts] [order(-OldVotes), .(Title, OldVotes)] [1:10]
    return(Final)
} #done