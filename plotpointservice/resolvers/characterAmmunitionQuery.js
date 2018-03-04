import {character_ammunition} from '../data'

export default function (obj, args, context, graphql) {

  return character_ammunition(obj.id)
}