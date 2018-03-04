import {hand_weapon_notes} from '../data'

export default function (obj, args, context, graphql) {
  return hand_weapon_notes(obj.id)
}