import {monstrous_abilities} from '../data'

export default function (obj, args, context, graphql) {

  return monstrous_abilities(obj.id)
}