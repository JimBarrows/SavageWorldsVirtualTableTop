import {character_hindrances} from '../data'

export default function (obj, args, context, graphql) {

  return character_hindrances(obj.id)
}