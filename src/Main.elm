module Main exposing (..)

import Html exposing (Html, button, div, img, program, text, textarea, Attribute)
import Html.Attributes exposing (src, value)
import Styles as Styles
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { 
        myselfId : String, 
        users : List User,
        talks: List Talk,
        inputText : String,
        nextTalkIdNum : Int
    }

type alias User =
    {
        id : String,
        name : String,
        imageUrl : String
    }

emptyUser : User
emptyUser =
    { id = ""
    , name = ""
    , imageUrl = ""
    }
    
type alias Talk =
    {
        id : String,
        userId : String,
        text : String,
        isEditing : Bool,
        createAt : String
    }

updateText : String -> Talk -> Talk
updateText newText talk =
    { talk | text = newText }

updateIsEditing : Bool -> Talk -> Talk
updateIsEditing isEditing talk =
    { talk | isEditing = isEditing }

type Msg
    = ChangeInput String
    | Add
    | Delete String
    | Edit String Bool
    | UpdateText String String

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
    { myselfId = "m1"
    , users = initUsers
    , talks = initTalks
    , inputText = ""
    , nextTalkIdNum = 3
    }

initUsers : List User
initUsers =
    [ User "m1" "とみざわ" "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg"
    , User "m2" "伊達ちゃん" "https://imgcp.aacdn.jp/img-c/680/auto/tipsplus/series/246/20160608_1465380998273.jpg"
    ]


initTalks : List Talk
initTalks =
    [ Talk "t1" "m2" "ピザ食いてえ" False "2018/01/27 13:00"
    , Talk "t2" "m1" "ちょっと何いってるかわかんないっす" False "2018/01/27 13:30"
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput text ->
            ( { model | inputText = text }, Cmd.none )
        
        Add -> 
            let
                nextTalkId =
                    "t" ++ toString model.nextTalkIdNum

                newTalk =
                    Talk nextTalkId model.myselfId model.inputText False "2018/01/27 14:00"
            in
                { model
                    | talks = model.talks ++ [ newTalk ]
                    , inputText = ""
                    , nextTalkIdNum = model.nextTalkIdNum + 1
                } ! []
        Delete id ->
            { model | talks = model.talks |> List.filter(\talk -> talk.id /= id )} ! []
        Edit id isEditing ->
            let
                updateTalk talk =
                    if talk.id == id then 
                        updateIsEditing isEditing talk
                    else
                        talk
            in
                { model | talks = List.map updateTalk model.talks } ! []
        UpdateText id text ->
            let
                updateTalk talk =
                    if talk.id == id then
                        updateText text talk
                    else 
                        talk       
            in
            { model | talks = List.map updateTalk model.talks } ! []
                
                


view : Model -> Html Msg
view model = 
    div [ Styles.mainWrap ]
        [ div [ Styles.postForm ]
            [ div [ Styles.formLeft ]
                [ img [ Styles.selfImg, src "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg" ] []
                ]
            , div [ Styles.formRight ]
              [ textarea [ Styles.formArea, value model.inputText, onInput ChangeInput ]  []
              , button [ Styles.postButton, onClick Add ] [ text "投稿！" ]
              ]
            ]
        , div [ Styles.talk ]
           <| List.map (\talk -> viewTalk talk model) model.talks
        ]


viewTalk : Talk -> Model -> Html Msg
viewTalk talk model = 
    let
        user =
           model.users
                |> List.filter(\user -> user.id == talk.userId )
                >> List.head
                >> Maybe.withDefault emptyUser
    in
    div [ Styles.talk ]
            [ div [ Styles.talkLeft ]
                [ img [ Styles.posterImg, src user.imageUrl ] [] ]
            , div [ Styles.talkRight ]
                [ div [ Styles.posterName ] [ text user.name ]
                , viewText model talk
                , div [ Styles.talkFooter ]
                    [ text talk.createAt
                    , viewEditButtons model talk ]
                ]
            ]

viewEditButtons : Model -> Talk -> Html Msg
viewEditButtons model talk =
    let
        editButtonText = 
            if talk.isEditing then
                "完了"
            else
                "編集"
    in
    if isMine model talk then
        div [ Styles.buttons ]
            [ button [ Styles.editButton, onClick <| Edit talk.id ( not talk.isEditing ) ] [ text editButtonText ]
            , button [ Styles.deleteButton, onClick <| Delete talk.id] [ text "削除"]
            ]
    else
        text ""

isMine : Model -> Talk -> Bool
isMine model talk =
    model.myselfId == talk.userId

viewText : Model -> Talk -> Html Msg
viewText model talk =
    if talk.isEditing then
        textarea [ Styles.editingMessage, value talk.text, onInput <| UpdateText talk.id, onEnter <| Edit talk.id False ] []
    else
        div [ Styles.message ] [ text talk.text ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        msgIfEnter key =
            if key == 13 then
                Json.succeed msg
            else 
                Json.fail "not enter"
    in
        on "keydown" (Json.andThen msgIfEnter keyCode)