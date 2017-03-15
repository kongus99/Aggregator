module LoginData exposing (LoginData, emptyLoginData, idGetter, selectUser, previewUser, setUser, setPossibleUsers, updateSteamUsername, updateGogUsername)

import Array exposing (Array)
import Model exposing (GameOn(Steam), User)


type alias LoginData =
    { loadedUser : Maybe User, typedUser : User, selectedUser : Int, possibleUsers : Array User }


emptyUser : User
emptyUser =
    User Nothing (Just "") False (Just "")


emptyLoginData =
    LoginData Nothing emptyUser -1 Array.empty


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
        { data | loadedUser = maybeUser, typedUser = maybeUser |> Maybe.withDefault data.typedUser, selectedUser = index }


previewUser whichLogin name data =
    let
        index =
            data.possibleUsers |> Array.toIndexedList |> List.filter (\( i, u ) -> idGetter whichLogin u == name) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault -1
    in
        { data | selectedUser = index }


setUser u data =
    { data | loadedUser = Just u, typedUser = u, selectedUser = -1, possibleUsers = Array.fromList [] }


setPossibleUsers users data =
    { data | selectedUser = 0, possibleUsers = Array.fromList users }


updateSteamUsername name data =
    { data | typedUser = (\user -> { user | steamUsername = Just name }) data.typedUser }


updateGogUsername name data =
    { data | typedUser = (\user -> { user | gogUsername = Just name }) data.typedUser }
