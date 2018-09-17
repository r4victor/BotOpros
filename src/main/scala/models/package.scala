package botopros

import botopros.helpers.State

package object models {
  type BotState = State[StateData, String]
}