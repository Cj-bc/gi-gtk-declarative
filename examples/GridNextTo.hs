{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GridNextTo where

import           Control.Monad                  ( void )
import           GI.Gtk                         ( Button(..)
                                                , Grid(..)
                                                , Label(..)
                                                , PositionType(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid

data State = State

data Event = Closed

view' :: State -> AppView Window Event
view' State =
  bin
      Window
      [ #title := "GridNextTo"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Grid
        [#rowSpacing := 4, #columnSpacing := 4, #margin := 4]
        [ GridChild
          { properties = GridChildPropertiesNextTo { width  = 3
                                                   , height = 3
                                                   , side   = PositionTypeBottom
                                                   }
          , child      = widget
                           Button
                           [ #label := "A"
                           , #hexpand := True
                           , #vexpand := True
                           ]
          }
        , GridChild
          { properties = GridChildPropertiesNextTo { width  = 3
                                                   , height = 1
                                                   , side   = PositionTypeRight
                                                   }
          , child      = widget Button [#label := "B"]
          }
        , GridChild
          { properties = GridChildPropertiesNextTo { width  = 2
                                                   , height = 2
                                                   , side   = PositionTypeLeft
                                                   }
          , child      = widget Button [#label := "C", #vexpand := True]
          }
        , GridChild
          { properties = GridChildPropertiesNextTo { width  = 1
                                                   , height = 1
                                                   , side   = PositionTypeBottom
                                                   }
          , child = widget Button
                           [#label := "D", #hexpand := True, #vexpand := True]
          }
        ]

update' :: State -> Event -> Transition State Event
update' State Closed = Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State
                      }
