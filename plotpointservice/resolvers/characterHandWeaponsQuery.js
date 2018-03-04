import {character_hand_weapons} from '../data'

export default function (obj, args, context, graphql) {

  return character_hand_weapons(obj.id)
}