import {makeExecutableSchema} from 'graphql-tools'
import {GraphQLServer} from 'graphql-yoga'
import config from './config'
import resolvers from './resolvers'
import typeDefs from './typeDefs'

const schema = makeExecutableSchema({typeDefs, resolvers})
const options = {
  cors: config.server.cors,
  tracing: config.server.tracing,
  port: config.server.port,
  endpoint: config.server.endpoint,
  subscriptions: config.server.subscriptions,
  playground: config.server.playground,
  uploads: config.server.uploads
}

const context = ({request}) => {
  return Object.assign({}, config.jwt)
}
const server = new GraphQLServer({
                                   schema: schema,
                                   context
                                 })
// server.express.post(server.options.endpoint, verifyToken)
server.start(options, () => console.log(`Server ${config.server.name} is running on ${config.server.url}`))