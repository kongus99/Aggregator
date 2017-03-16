module LoginData exposing (LoginData, emptyLoginData, idGetter, selectUser, setUser, setPossibleUsers, updateSteamUsername, updateGogUsername, changeActiveUsername, filterUsers)

import Array exposing (Array)
import Model exposing (GameOn(Steam), User)


type alias LoginData =
    { userLoaded : Bool, user : User, possibleUsers : Array User, activeUsername : Maybe GameOn }


emptyUser : User
emptyUser =
    User Nothing (Just "") False (Just "")


emptyLoginData : LoginData
emptyLoginData =
    LoginData False emptyUser Array.empty Nothing


idGetter : Maybe GameOn -> User -> String
idGetter activeInput user =
    activeInput
        |> Maybe.andThen
            (\i ->
                if i == Steam then
                    user.steamUsername
                else
                    user.gogUsername
            )
        |> Maybe.withDefault ""


selectUser : String -> LoginData -> LoginData
selectUser name data =
    let
        index =
            data.possibleUsers
                |> Array.toIndexedList
                |> List.filter (\( i, u ) -> idGetter data.activeUsername u == name)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1

        maybeUser =
            data.possibleUsers
                |> Array.get index
    in
        { data | userLoaded = True, user = maybeUser |> Maybe.withDefault data.user, activeUsername = Nothing }


setUser : User -> LoginData -> LoginData
setUser u data =
    { data | userLoaded = True, user = u, possibleUsers = Array.fromList [] }


setPossibleUsers : List User -> LoginData -> LoginData
setPossibleUsers users data =
    { data | possibleUsers = Array.fromList users }


updateSteamUsername : String -> LoginData -> LoginData
updateSteamUsername name data =
    { data | user = (\user -> { user | steamUsername = Just name }) data.user }


updateGogUsername : String -> LoginData -> LoginData
updateGogUsername name data =
    { data | user = (\user -> { user | gogUsername = Just name }) data.user }


changeActiveUsername : GameOn -> LoginData -> LoginData
changeActiveUsername active data =
    { data
        | activeUsername =
            if data.userLoaded then
                Nothing
            else
                Just active
    }


filterUsers : LoginData -> List User
filterUsers data =
    let
        gogUsername =
            data.user.gogUsername |> Maybe.withDefault ""

        matchesGogUsername u =
            u.gogUsername |> Maybe.map (\n -> String.contains gogUsername n) |> Maybe.withDefault True

        steamUsername =
            data.user.steamUsername |> Maybe.withDefault ""

        matchesSteamUsername u =
            u.steamUsername |> Maybe.map (\n -> String.contains steamUsername n) |> Maybe.withDefault True
    in
        data.possibleUsers |> Array.filter (\u -> matchesGogUsername u && matchesSteamUsername u) |> Array.toList
