import {hand_weapon_category} from '../data'

export default function (obj, args, context, graphql) {
  return hand_weapon_category(obj.id)
}