import {character_powers} from '../data'

export default function (obj, args, context, graphql) {
  return character_powers(obj.id)
}