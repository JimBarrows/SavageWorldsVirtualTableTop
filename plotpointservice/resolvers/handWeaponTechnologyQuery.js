import {hand_weapon_technology_level} from '../data'

export default function (obj, args, context, graphql) {
  return hand_weapon_technology_level(obj.id)
}