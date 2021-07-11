module TextCreator exposing (..)

{-
   Copyright 2017-2019 Christopher Kumar Anand,  Adele Olejarz, Chinmay Sheth, Yaminah Qureshi, Graeme Crawley and students of McMaster University.  Based on the Shape Creator by Levin Noronha.

      Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

      1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

      2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution, and cite the paper

      @article{d_Alves_2018,
      title={Using Elm to Introduce Algebraic Thinking to K-8 Students},
      volume={270},
      ISSN={2075-2180},
      url={http://dx.doi.org/10.4204/EPTCS.270.2},
      DOI={10.4204/eptcs.270.2},
      journal={Electronic Proceedings in Theoretical Computer Science},
      publisher={Open Publishing Association},
      author={d’ Alves, Curtis and Bouman, Tanya and Schankula, Christopher and Hogg, Jenell and Noronha, Levin and Horsman, Emily and Siddiqui, Rumsha and Anand, Christopher Kumar},
      year={2018},
      month={May},
      pages={18–36}
      }

      3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR AN, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import GraphicSVG.Widget exposing (Model)
import Html exposing (th)
import List
import ShapeCreateAssets exposing (..)
import String exposing (..)


init =
    { myshape = MyCircle
    , recurcounter = 1.0
    , selectedRec = 0
    , move_x = 0
    , move_y = 0
    , mycurrentbut = None
    , location = ( -237, -77 )
    , myscale = 1
    , myrotate = 0
    , recurMyshape = True
    , recurMylist = False
    , time = 0
    , notify = NotifyTap
    , style = Solid
    , lineWidth = 1
    , width = 10
    , height = 15
    , txt = ""
    , clr = RGB
    , red = 100
    , green = 0
    , blue = 100
    , keyboard = 1
    , txtsize = 14
    , currentButton = None
    , buttonDownTime = 0
    , deleteButtondown = 0
    , nextButtonDown = 0
    }


type Msg m
    = Tick Float GetKeyState
    | LStyle
    | SetColour Colour
    | TransM (m -> m)
    | Key String
    | ClearOne
    | Tab
    | ButtonDown ButtonDir
    | MouseUp
    | ChangeShape Shapes
    | ChangeValue NumDir


type ButtonDir
    = RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | Delete
    | None
    | ValueUp
    | ValueDown


type NumDir
    = NumUp
    | NumDown


type Shapes
    = MyCircle
    | MyRectangle
    | MyTriangle
    | MyText


update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , deleteButtondown =
                    case model.currentButton of
                        Delete ->
                            accel model.buttonDownTime

                        _ ->
                            0
                , nextButtonDown =
                    case model.currentButton of
                        Delete ->
                            model.nextButtonDown

                        _ ->
                            0
                , recurcounter =
                    if model.selectedRec == 0 then
                        case model.currentButton of
                            ValueUp ->
                                if model.recurcounter < 50 then
                                    model.recurcounter + valueChaning model.buttonDownTime

                                else
                                    model.recurcounter

                            ValueDown ->
                                if model.recurcounter > 1 then
                                    model.recurcounter - valueChaning model.buttonDownTime

                                else
                                    model.recurcounter

                            _ ->
                                model.recurcounter

                    else
                        model.recurcounter
                , myscale =
                    if model.selectedRec == 3 then
                        case model.currentButton of
                            ValueUp ->
                                if model.myscale < 10 then
                                    model.myscale + valueChaning model.buttonDownTime

                                else
                                    model.myscale

                            ValueDown ->
                                if model.myscale > 0 then
                                    model.myscale - valueChaning model.buttonDownTime

                                else
                                    model.myscale

                            _ ->
                                model.myscale

                    else
                        model.myscale
                , move_x =
                    if model.selectedRec == 1 then
                        case model.currentButton of
                            ValueUp ->
                                model.move_x + valueChaning model.buttonDownTime

                            ValueDown ->
                                model.move_x - valueChaning model.buttonDownTime

                            _ ->
                                model.move_x

                    else
                        model.move_x
                , move_y =
                    if model.selectedRec == 2 then
                        case model.currentButton of
                            ValueUp ->
                                model.move_y + valueChaning model.buttonDownTime

                            ValueDown ->
                                model.move_y - valueChaning model.buttonDownTime

                            _ ->
                                model.move_y

                    else
                        model.move_y
                , myrotate =
                    if model.selectedRec == 4 then
                        case model.currentButton of
                            ValueUp ->
                                model.myrotate + valueChaning model.buttonDownTime

                            ValueDown ->
                                model.myrotate - valueChaning model.buttonDownTime

                            _ ->
                                model.myrotate

                    else
                        model.myrotate
            }
                |> (if model.deleteButtondown > 2 then
                        \m -> { m | txt = dropRight 1 model.txt }

                    else
                        \m -> m
                   )

        MouseUp ->
            { model | currentButton = None }

        ButtonDown dir ->
            { model | currentButton = dir }

        LStyle ->
            { model
                | style =
                    case model.style of
                        Solid ->
                            Dotted

                        Dotted ->
                            Dashed

                        Dashed ->
                            Longdash

                        Longdash ->
                            Dotdash

                        Dotdash ->
                            Dotted
            }

        TransM t ->
            t model

        SetColour clr ->
            { model | clr = clr }

        Key s ->
            { model
                | txt = setTxt model s
                , myshape = MyText
            }

        Tab ->
            { model
                | keyboard =
                    case model.keyboard of
                        1 ->
                            2

                        2 ->
                            3

                        3 ->
                            4

                        _ ->
                            1
            }

        ClearOne ->
            { model | txt = dropRight 1 model.txt }

        ChangeShape MyCircle ->
            { model | myshape = MyCircle }

        ChangeShape MyRectangle ->
            { model | myshape = MyRectangle }

        ChangeShape MyTriangle ->
            { model | myshape = MyTriangle }

        ChangeShape MyText ->
            { model | myshape = MyText }

        ChangeValue NumUp ->
            { model
                | recurcounter =
                    case model.selectedRec of
                        0 ->
                            if model.recurcounter <= 50 then
                                model.recurcounter + 1

                            else
                                model.recurcounter

                        _ ->
                            model.recurcounter
                , move_x =
                    case model.selectedRec of
                        1 ->
                            model.move_x + 1

                        _ ->
                            model.move_x
                , move_y =
                    case model.selectedRec of
                        2 ->
                            model.move_y + 1

                        _ ->
                            model.move_y
                , myscale =
                    case model.selectedRec of
                        3 ->
                            if model.myscale < 5 then
                                model.myscale + 0.25

                            else
                                model.myscale

                        _ ->
                            model.myscale
                , myrotate =
                    case model.selectedRec of
                        4 ->
                            model.myrotate + 5

                        _ ->
                            model.myrotate
            }

        ChangeValue NumDown ->
            { model
                | recurcounter =
                    case model.selectedRec of
                        0 ->
                            if model.recurcounter > 1 then
                                model.recurcounter - 1

                            else
                                model.recurcounter

                        _ ->
                            model.recurcounter
                , move_x =
                    case model.selectedRec of
                        1 ->
                            model.move_x - 1

                        _ ->
                            model.move_x
                , move_y =
                    case model.selectedRec of
                        2 ->
                            model.move_y - 1

                        _ ->
                            model.move_y
                , myscale =
                    case model.selectedRec of
                        3 ->
                            if model.myscale > 0 then
                                model.myscale - 0.25

                            else
                                model.myscale

                        _ ->
                            model.myscale
                , myrotate =
                    case model.selectedRec of
                        4 ->
                            model.myrotate - 5

                        _ ->
                            model.myrotate
            }


view model =
    [ graphPaperCustom 10 1 (rgb 50 250 130) |> makeTransparent 0.5 -- axes and selected coordinate ticks
    , shapeFun model -- To draw the different shapes in the middle of the canvas
    , keyboard model |> scale 0.8 |> move ( -230, 146 ) --Keyboard for inputting the letters
    , shapePicker |> scale 0.8 |> move ( -50, 25 ) --Left side of the screen, used for letting user to pick the shapes they want to use
    , yourCode model |> move ( 130, -110 ) --Right bottom, showing the code to the user
    , controlPanel model |> move ( -240, -20 ) -- Counter, Move, Scale etc.. The panels to control recursion
    , group (blinkRectangle model) -- Make the selected paramater blink
    , group [ keypad model ] -- The upside triangle and downside triangle to increase and decrease the value of each seleteced parameter
    ]



{-
   @parameter: None
   @function: Created shapes at the right side of the canvas, users can choose what shape they want visually by click those shapes
   @Return: Shape
-}


shapePicker =
    group
        [ circle 20
            |> filled green
            |> notifyTap (ChangeShape MyCircle)
            |> move ( -220, 30 )
        , rect 40 40
            |> filled green
            |> move ( -160, 30 )
            |> notifyTap (ChangeShape MyRectangle)
        , triangle 30
            |> filled green
            |> rotate (degrees 30)
            |> move ( -95, 35 )
            |> notifyTap (ChangeShape MyTriangle)
        ]



{-
   @parameter: model
   @function: intergrate together the control panel for Counter, Move, Scale, Rotate etc
   @Return: A list of shapes
-}


controlPanel m =
    group
        [ text "RecursiveShapes"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( -10, 15 )
            |> notifyTap (TransM (\model -> { model | recurMyshape = not model.recurMyshape }))
        , text "RecursiveList"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( -10, -15 )
            |> notifyTap (TransM (\model -> { model | recurMylist = not model.recurMylist }))
        , counterControl m |> move ( -10, -60 )
        , moveControl m |> move ( 100, -60 )
        , scaleControl m |> move ( -10, -120 )
        , rotateControl m |> move ( 100, -120 )
        ]



{-
   @parameter: model
   @function: The upside triangle and downside triangle for increasing the value and decreasing the value of each selected parameter
   @Return: A list of shapes
-}


keypad model =
    group
        [ triangle 12
            |> filled lightGreen
            |> rotate (degrees -30)
            |> move ( 55, 20 )
            |> notifyTap (ChangeValue NumUp)
            |> notifyMouseDown (ButtonDown ValueUp)
            |> notifyMouseUp (ButtonDown None)
        , triangle 12
            |> filled lightGreen
            |> rotate (degrees 30)
            |> move ( 55, -15 )
            |> notifyTap (ChangeValue NumDown)
            |> notifyMouseDown (ButtonDown ValueDown)
            |> notifyMouseUp (ButtonDown None)
        ]
        |> move model.location



-- The keypad should move to whereever the selected parameter is
{-
   @parameter: model
   @function: Control panel for Counter
   @Return: A list of shapes
-}


counterControl m =
    group
        [ text "Counter"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 0, 0 )
        , text (String.fromFloat m.recurcounter)
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 60, 0 )
        ]



{-
   @parameter: x position of the rectangle, y position of the rectangle, color of the rectangle
   @function: Create a rectangle, later will be used for blinking on the selected parameter
   @Return: Shape
-}


invisibleRectangle x y b =
    group [ rect 20 25 |> filled b |> move ( x, y ) ]



{-
   @parameter: model
   @function: Using constanct location to create a rectangle to blink when the parameter is selected, and each location is bound with notifyTap
   @Return: A list of shapes
-}


blinkRectangle model =
    let
        invisibleLocation =
            -- Invisible locations for Counter, Move, Scale etc..
            [ ( ( -182, -75 ), 0 )
            , ( ( -82, -75 ), 1 )
            , ( ( -43, -75 ), 2 )
            , ( ( -188, -137 ), 3 )
            , ( ( -77, -137 ), 4 )
            ]
    in
    List.map
        (\( ( x, y ), selectedone ) ->
            invisibleRectangle x y lightOrange
                |> makeTransparent
                    -- Make the selected rectangle blink
                    (if model.selectedRec == selectedone then
                        abs (sin model.time) * 0.5

                     else
                        0
                    )
                |> notifyTap
                    -- Update the model.selectedRec whenever a certain parameter is selected
                    (TransM
                        (\m ->
                            { m
                                | selectedRec = selectedone
                                , location = ( x - 55, y - 2 )
                            }
                        )
                    )
        )
        invisibleLocation



{-
   @parameter: model
   @function: Control panel for Move
   @Return: A list of shapes
-}


moveControl m =
    group
        [ text "Move ("
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 0, 0 )
        , text (String.fromFloat m.move_x)
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 50, 0 )
        , text ","
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 70, 0 )
        , text (String.fromFloat m.move_y)
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 90, 0 )
        , text ")"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 110, 0 )
        ]



{-
   @parameter: model
   @function: Control panel for Scale
   @Return: A list of shapes
-}


scaleControl m =
    group
        [ text "Scale"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 0, 0 )
        , text (String.fromFloat m.myscale)
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 55, 0 )
        ]


rotateControl m =
    group
        [ text "Rotate"
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 0, 0 )
        , text (String.fromFloat m.myrotate)
            |> fixedwidth
            |> size 13
            |> filled black
            |> move ( 55, 0 )
        ]



{-
   @parameter: model
   @function: checke if user want to create a shape recursively
   @Return: function to create shapes recursively
-}


recurShape model =
    if model.recurMyshape then
        recursiveShape model model.recurcounter

    else
        \x -> x



{-
   @parameter: model
   @function: checke if user want to create each leter in a list recursively
   @Return: function to create letters in a list recursively
-}
{-
   @parameter1: model
   @parameter2: counter - float
   @parameter3: myshape - Shape
   @function: to create shapes recursively, and let use tweak on each shape
   @Return: A group of shapes
-}


recursiveShape model counter myshape =
    (if counter == 0 then
        []

     else
        [ myshape
        , recursiveShape model (counter - 1) myshape
            |> move ( model.move_x, model.move_y )
            |> scale model.myscale
            |> rotate (degrees model.myrotate)
        ]
    )
        |> group



{-
   @parameter1: model
   @parameter2: letter - a string
   @function: to create each letter in a string recursively
   @Return: a group of list
-}


recursiveList model letters =
    case letters of
        letter1 :: moreLetters ->
            [ text letter1 |> centered |> fixedwidth |> filled black
            , recursiveList model moreLetters
                |> scale model.myscale
                |> rotate model.myrotate
                |> move ( model.move_x, model.move_y )
            ]
                |> group

        [] ->
            group []


type Colour
    = RGB


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type LineStyle
    = Solid
    | Dotted
    | Dashed
    | Longdash
    | Dotdash


cyan =
    rgb 0 150 150


setTxt m s =
    m.txt ++ s


colourAmount =
    3



-- this case catches every other string and turns it into Hello
-- since there are an infinite number of Strings, we need a catch-all case
-- main view components


keyboard model =
    group
        [ rect 170 120 |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey |> move ( 60, -23 )
        , rect 89 12 |> filled white |> addOutline (solid 1) lightGrey |> move ( 60, 37 )
        , text "1. Choose Your Text!" |> serif |> italic |> size 10 |> filled titleColour |> move ( 18, 34 )
        , rect 20 20 |> filled red

        -- , text "X" |> filled black |> move (113, -60) |> notifyTap (Clear)
        , rect 20 20 |> filled white |> notifyTap Tab |> move ( 100, -57 )
        , text "Aa" |> filled grey |> move ( 93, -60 ) |> notifyTap Tab
        , if model.keyboard <= 2 then
            group <|
                List.map2
                    (\y z ->
                        rect 20 25 |> filled cyan |> move ( y, z + 6 )
                    )
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                    (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 25))

          else
            group <|
                List.map2
                    (\y z ->
                        rect 20 25 |> filled cyan |> move ( y, z + 6 )
                    )
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 20))
                    (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 20))
        , if model.keyboard == 1 then
            group <|
                List.map3
                    (\ss y z ->
                        text ss |> centered |> size 18 |> filled white |> move ( y, z )
                    )
                    [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                    (List.map (\x -> Basics.toFloat (-20 * (x // 7))) (List.range 0 25))
                    ++ List.map3
                        (\y z ss ->
                            rect 20 25 |> filled white |> move ( y, z + 6 ) |> makeTransparent 0 |> notifyTap (Key ss)
                        )
                        (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                        (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 25))
                        [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]

          else if model.keyboard == 2 then
            group <|
                List.map3
                    (\ss y z ->
                        text ss |> centered |> size 18 |> filled white |> move ( y, z )
                    )
                    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                    (List.map (\x -> Basics.toFloat (-20 * (x // 7))) (List.range 0 25))
                    ++ List.map3
                        (\y z ss ->
                            rect 20 25 |> filled white |> move ( y, z + 6 ) |> makeTransparent 0 |> notifyTap (Key ss)
                        )
                        (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                        (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 25))
                        [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]

          else if model.keyboard == 3 then
            group <|
                List.map3
                    (\ss y z ->
                        text ss |> centered |> size 18 |> filled white |> move ( y, z )
                    )
                    [ "`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "[", "]", "\"", ";", "'", ",", ".", "/" ]
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                    (List.map (\x -> Basics.toFloat (-20 * (x // 7))) (List.range 0 25))
                    ++ List.map3
                        (\y z ss ->
                            rect 20 25 |> filled white |> move ( y, z + 6 ) |> makeTransparent 0 |> notifyTap (Key ss)
                        )
                        (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 20))
                        (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 20))
                        [ "`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "[", "]", "\"", ";", "'", ",", ".", "/" ]
                    ++ [ rect 100 25 |> filled cyan |> move ( 40, Basics.toFloat (-21 * (21 // 7)) + 6 )
                       , text "SPACE" |> filled white |> move ( 20, Basics.toFloat (-21 * (21 // 7)) )
                       , rect 100 25 |> filled white |> makeTransparent 0 |> move ( 40, Basics.toFloat (-21 * (21 // 7)) + 6 ) |> notifyTap (Key " ")
                       ]

          else
            group <|
                List.map3
                    (\ss y z ->
                        text ss |> centered |> size 18 |> filled white |> move ( y, z )
                    )
                    [ "~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+", "{", "}", "|", "", ":", "<", ">", "?" ]
                    -- missing "
                    (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 25))
                    (List.map (\x -> Basics.toFloat (-20 * (x // 7))) (List.range 0 25))
                    ++ List.map3
                        (\y z ss ->
                            rect 20 25 |> filled white |> move ( y, z + 6 ) |> makeTransparent 0 |> notifyTap (Key ss)
                        )
                        (List.map (\x -> Basics.toFloat (20 * (x |> modBy 7))) (List.range 0 20))
                        (List.map (\x -> Basics.toFloat (-21 * (x // 7))) (List.range 0 20))
                        [ "~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "+", "{", "}", "|", "", ":", "<", ">", "?" ]
        , group [ circle 10 |> filled red, rect 5 15 |> filled white |> rotate (degrees 45), rect 5 15 |> filled white |> rotate (degrees -45) ]
            |> notifyTap ClearOne
            |> move ( 120, -57 )
            |> notifyMouseDown (ButtonDown Delete)
            |> notifyMouseUp (ButtonDown None)
        ]


valueChaning x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


yourCode m =
    group
        [ rect 250 180 |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey |> move ( 15, -75 )
        , rect 80 12 |> filled white |> addOutline (solid 1) lightGrey |> move ( -55, 14 )
        , text "Your code!" |> serif |> italic |> size 10 |> filled titleColour |> move ( -90, 11 )
        , "This is ***********" |> copiable |> move ( -105, 0 )
        , "recursiveShape model counter myshape = " |> copiable |> move ( -105, -20 )
        , " ( if " ++ "conter" ++ "== 0 then" |> copiable |> move ( -105, -30 )
        , "     []" |> copiable |> move ( -105, -40 )
        , "   else" |> copiable |> move ( -105, -50 )
        , "     [ " ++ shapeToString m |> copiable |> move ( -105, -60 )
        , "     , recursiveShape model " ++ "(counter - 1))" ++ " " ++ shapeToString m |> copiable |> move ( -105, -70 )
        , "         |> move ( " ++ String.fromFloat m.move_x ++ " , " ++ String.fromFloat m.move_y ++ " )" |> copiable |> move ( -105, -80 )
        , "         |>scale " ++ String.fromFloat m.myscale |> copiable |> move ( -105, -90 )
        , "         |>rotate " ++ "( degrees 30)" |> copiable |> move ( -105, -100 )
        , "     ]" |> copiable |> move ( -105, -110 )
        , " )" |> copiable |> move ( -105, -120 )
        , " |> group" |> copiable |> move ( -105, -130 )
        ]
        |> move ( 0, 70 )


shapeToString m =
    case m.myshape of
        MyCircle ->
            "Circle"

        MyRectangle ->
            "Rectangle"

        MyTriangle ->
            "Triangle"

        MyText ->
            "Text"


shapeFun m =
    if m.recurMylist then
        recursiveList m (m.txt |> String.toList |> List.map String.fromChar)

    else
        (case m.myshape of
            MyText ->
                text m.txt
                    |> fixedwidth
                    |> size 13
                    |> filled black

            MyCircle ->
                circle 30
                    |> outlined (solid 1) red
                    |> move ( 0, 50 )

            MyRectangle ->
                rect 40 40
                    |> outlined (solid 1) red
                    |> move ( 0, 50 )

            MyTriangle ->
                triangle 40
                    |> outlined (solid 1) red
                    |> rotate (degrees 30)
                    |> move ( 0, 50 )
        )
            |> recurShape m


titleColour =
    cyan


code str =
    str |> text |> fixedwidth |> size 9 |> filled black
