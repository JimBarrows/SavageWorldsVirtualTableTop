import {character_mundane_items} from '../data'

export default function (obj, args, context, graphql) {

  return character_mundane_items(obj.id)
}