module LoginData exposing (LoginData, emptyLoginData, idGetter, selectUser, previewUser, setUser, setPossibleUsers, updateSteamUsername, updateGogUsername)

import Array exposing (Array)
import Model exposing (GameOn(Steam), User)


type alias LoginData =
    { userLoaded : Bool, user : User, selectedUser : Int, possibleUsers : Array User }


emptyUser : User
emptyUser =
    User Nothing (Just "") False (Just "")


emptyLoginData : LoginData
emptyLoginData =
    LoginData False emptyUser -1 Array.empty


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


selectUser : Maybe GameOn -> String -> LoginData -> LoginData
selectUser whichLogin name data =
    let
        index =
            data.possibleUsers
                |> Array.toIndexedList
                |> List.filter (\( i, u ) -> idGetter whichLogin u == name)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1

        maybeUser =
            data.possibleUsers
                |> Array.get index
    in
        { data | userLoaded = True, user = maybeUser |> Maybe.withDefault data.user, selectedUser = index }


previewUser : Maybe GameOn -> String -> LoginData -> LoginData
previewUser whichLogin name data =
    let
        index =
            data.possibleUsers |> Array.toIndexedList |> List.filter (\( i, u ) -> idGetter whichLogin u == name) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault -1
    in
        { data | selectedUser = index }


setUser : User -> LoginData -> LoginData
setUser u data =
    { data | userLoaded = True, user = u, selectedUser = -1, possibleUsers = Array.fromList [] }


setPossibleUsers : List User -> LoginData -> LoginData
setPossibleUsers users data =
    { data | selectedUser = 0, possibleUsers = Array.fromList users }


updateSteamUsername : String -> LoginData -> LoginData
updateSteamUsername name data =
    { data | user = (\user -> { user | steamUsername = Just name }) data.user }


updateGogUsername : String -> LoginData -> LoginData
updateGogUsername name data =
    { data | user = (\user -> { user | gogUsername = Just name }) data.user }
