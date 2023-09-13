module Page.Chat.Message exposing
    ( Message(..)
    , ActiveUser
    )

{-|

@docs Message
@docs ActiveUser

-}


{-| Type safe chat message.

  - `UserEntered`: A message indicating that a user has entered the room.
      - `user`: The user who has entered.
      - `activeUsers`: All of the users who are in the chat room at the moment.
  - `UserLeft`: A message indicating that a user has left the room.
      - `user`: The user who has left.
      - `activeUsers`: All of the users who are in the chat room at the moment.
  - `UserMessage`: A message from other users.
      - `user`: The user who sent the message.
      - `message`: The body of the message.

-}
type Message
    = UserEntered
        { user : ActiveUser
        , activeUsers : List ActiveUser
        }
    | UserLeft
        { user : ActiveUser
        , activeUsers : List ActiveUser
        }
    | UserMessage
        { user : ActiveUser
        , message : String
        }


{-| Represent a user who is in the chat room at the moment.

  - `displayName`: Display name for the user.
  - `color`: Color for the user name.

-}
type alias ActiveUser =
    { displayName : String
    , color : String
    }
