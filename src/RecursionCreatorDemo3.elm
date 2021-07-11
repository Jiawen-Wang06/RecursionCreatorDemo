myShapes model =
  [
    text "RecurRotate"
    |> filled (rgb 100 125 50)
    |> scale 0.75
    |> move (-95,-10)
    |> notifyTap (Toggle RecurRotate)
    ,
    text "recursion"
    |> filled (rgb 100 125 50)
    |> scale 0.75
    |> move (-95,-20)
    |> notifyTap (Toggle Recur)
    ,
    text "Scale"
    |> filled (rgb 230 125 50)
    |> scale 0.75
    |> move (-95,-30)
    |> notifyTap (Toggle Scale)
    ,
    text "move"
    |> filled (rgb 200 125 50)
    |> scale 0.75
    |> move (-95,-40)
    |> notifyTap (Toggle Move)
    ,
    controlCounterView model
    ,
    shapeButtom
    , 
    shapeFun model
  ]
  
init = { time = 0,
         scaleIt = False,
         moveIt = False,
         recurIt = False,
         recurRot = False,
         shape = Circle,
         counter =2.0,
         degree = 30 }
  
type Transforms
    = Scale
    | Move
    | Recur
    | RecurRotate
    
type Shapes
    = Circle
    | Letter
    | Car
    | Coral
    | Rectangle

type Direction
    = Increase
    | Decrease

    
type Msg m 
          = Tick Float GetKeyState
          |Toggle Transforms
          |ChangeShape Shapes
          |ControlCounter Direction

shapeButtom = group
    [
     circle 4
      |> filled pink
      |> notifyTap (ChangeShape Circle)
      |> move (-90, -55)
      ,
     text "A"
      |> filled pink
      |> notifyTap (ChangeShape Letter)
      |> move (-83,-60)
      ,
     car
      |> scale 0.2
      |> notifyTap (ChangeShape Car)
      |> move (-60,-58)
      ,
     coral 10
      |> scale 0.2
      |> notifyTap (ChangeShape Coral)
      |> move (-40,-60)
      ,
     rect 6 6
      |> filled pink 
      |> move (-30,-55)
      |> notifyTap (ChangeShape Rectangle)
       
    ]

controlCounterView model = group
  [
    text "+" 
    |> outlined (solid 1) black 
    |> scale 0.7
    |> notifyTap (ControlCounter Increase)
    |> move (-60, -21)
    ,
    text (String.fromFloat model.counter)
    |> filled black 
    |> scale 1
    |> move (-54, -22)
    ,
    text "-"
    |> outlined (solid 1) black 
    |> scale 0.7
    |> notifyTap (ControlCounter Decrease)
    |> move (-40, -21)
  ]

coral size =
  (polygon [(-0.5*size,0),(0.5*size,0),(0.3*size,3*size),(-0.25*size,3.1*size)]
     |> filled (rgb 255 (5*size+100) (4*size+100)) )
  :: ( if size > 0.5 then
         [ coral (0.55*size) |> rotate (degrees -40) |> move (0.1*size,size*2.9)
         , coral (0.4*size) |> rotate (degrees -40) |> move (0.3*size,size*1.5)
         , coral (0.5*size) |> rotate (degrees 40) |> move (-0.1*size,size*3.0)
         , coral (0.35*size) |> rotate (degrees 60) |> move (-0.2*size,size*1.9)
         , coral (0.45*size) |> rotate (degrees 70) |> move (-0.2*size,size*1.1)
         ]
         
       else
         []
     )
     |> group 
     
fish = group
  [
  
          oval 50 30
          |> filled red
      ,   ngon 3 20
          |> filled red 
          |> move (-30,0)
      ,   curve (0,0) [Pull (10,0) (20,-10)]
          |> filled black
          |> scale 0.5
          |> rotate (degrees 180)
          |> move (20,-10)
      ,   circle 3 
          |> filled black
          |> move (12,5)
      ,   wedge 10 0.25
          |> filled lightYellow 
          |> rotate (degrees 180)
  ]
  
car = group
  [
    
       roundedRect 100 25 20
          |> filled blue
      , wedge 30 0.5
          |> filled blue
          |> rotate (degrees 90)
          |> move (0,8)
      , wedge 5 0.5
          |> filled yellow
          |> move(48,0)
      , wedge 5 0.5
          |> filled red
          |> rotate (degrees 180)
          |> move(-48,0)
      , circle 10
          |> filled black
          |> move(-20,-10)
      , circle 10
          |> filled black
          |> move(20,-10)
      , wedge 20 0.5
          |> filled white
          |> rotate (degrees 90)
          |> move(0,12)
      , circle 5
          |> filled white
          |> move(20,-10)
      , circle 5
          |> filled white
          |> move(-20,-10)
  ]
  
shapeFun model = 
  (case model.shape of
    Circle ->
      circle 10 |> outlined (solid 1) red |> move(0,0)
    Coral ->
      coral 10 |>scale 0.8 |> move (0, 0)
    Letter ->
      text "A" |> filled pink |> scale 2 |> move(0,0)
    Car ->
      car |> scale 0.3 |> move(0,0)
    Rectangle ->
      rect 40 40 |> outlined (solid 0.5) (rgb 230 125 50) |> move (0,0)
   )
     |> scaleShape model 
     |> moveShape model
     |> recurShape model
     |> recurRotate model


update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     Toggle Scale -> {model | scaleIt = not model.scaleIt}
                     Toggle Move -> {model | moveIt = not model.moveIt}
                     Toggle Recur -> {model | recurIt = not model.recurIt}
                     Toggle RecurRotate -> {model | recurRot = not model.recurRot}
                     ChangeShape Circle -> {model | shape = Circle}
                     ChangeShape Coral -> {model | shape = Coral}
                     ChangeShape Letter -> {model | shape = Letter}
                     ChangeShape Car -> {model | shape = Car}
                     ChangeShape Rectangle -> {model | shape = Rectangle}
                     ControlCounter Increase -> 
                        let 
                          upperbound = (model.counter-50)
                        in
                          if upperbound < 0 then
                            {
                              model | counter = model.counter + 1
                            }
                          else
                            {
                              model | counter = model.counter
                            }
                     ControlCounter Decrease -> 
                        let 
                          lowerbound = (model.counter-2)
                        in
                          if lowerbound > 0 then
                            {
                              model | counter = model.counter-1
                            }
                          else
                            {
                              model | counter = model.counter
                            }
                      
                     

scaleShape model = 
  if model.scaleIt then
    scale 2
  else
    \x -> x
    
moveShape model = 
  if model.moveIt then
    move(50,10)
  else
    \x -> x
    
recurShape model = 
  if model.recurIt then
    recursiveShape model.counter
  else
    \x -> x

recurRotate model = 
  if model.recurRot then  
    recursiveRotate model.degree model.counter
  else  
   \x -> x


recursiveShape counter shape = group
  [
    if counter == 0 then
      group[]
    else
      group
        [
          shape
          |> move(20*sin(counter+3.14), 0)
          ,
          recursiveShape (counter-1) shape
        ]

  ]

recursiveRotate degree counter shape  = group
  [
    if counter == 0 then
     group[]
    else
      group
        [
          shape
            |> rotate (degrees degree)
          ,
          recursiveRotate (degree*1.5) (counter-1) shape 
        ]

  ]

 
                     


***********
recursiveShape model counter myshape =
    (if counter == 0 then
        []

     else
        [ myshape
        , recursiveShape model (counter - 1) myshape
            |> move ( model.move_x, model.move_y )
            |> scale model.myscale
            |> rotate (degrees 30)
        ]
    )
        |> group


type Shapes
    = MyCircle
    | MyRectangle
    | MyTriangle
    | MyText