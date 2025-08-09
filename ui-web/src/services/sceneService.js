import { API, graphqlOperation } from 'aws-amplify'

// GraphQL operations (these would normally be auto-generated)
const listScenes = `
  query ListScenes {
    listScenes {
      items {
        id
        name
        description
        dramatis_personae {
          name
          description
        }
      }
    }
  }
`

const getScene = `
  query GetScene($id: ID!) {
    getScene(id: $id) {
      id
      name
      description
      dramatis_personae {
        name
        description
      }
    }
  }
`

const createScene = `
  mutation CreateScene($input: CreateSceneInput!) {
    createScene(input: $input) {
      id
      name
      description
      dramatis_personae {
        name
        description
      }
    }
  }
`

const updateScene = `
  mutation UpdateScene($input: UpdateSceneInput!) {
    updateScene(input: $input) {
      id
      name
      description
      dramatis_personae {
        name
        description
      }
    }
  }
`

const deleteScene = `
  mutation DeleteScene($input: DeleteSceneInput!) {
    deleteScene(input: $input) {
      id
    }
  }
`

const sceneService = {
  async listScenes() {
    try {
      const response = await API.graphql(graphqlOperation(listScenes))
      return response.data.listScenes.items || []
    } catch (error) {
      throw error
    }
  },

  async getScene(id) {
    if (!id || id.trim() === '') {
      throw new Error('Scene ID is required')
    }

    try {
      const response = await API.graphql(graphqlOperation(getScene, { id }))
      return response.data.getScene
    } catch (error) {
      throw error
    }
  },

  async createScene(scene) {
    if (!scene) {
      throw new Error('Scene is required')
    }
    if (!scene.name || scene.name.trim() === '') {
      throw new Error('Scene name is required')
    }

    try {
      const input = {
        name: scene.name,
        description: scene.description || '',
        dramatis_personae: scene.dramatis_personae || []
      }
      
      const response = await API.graphql(graphqlOperation(createScene, { input }))
      return response.data.createScene
    } catch (error) {
      throw error
    }
  },

  async updateScene(scene) {
    if (!scene) {
      throw new Error('Scene is required')
    }
    if (!scene.id || scene.id.trim() === '') {
      throw new Error('Scene ID is required')
    }
    if (!scene.name || scene.name.trim() === '') {
      throw new Error('Scene name is required')
    }

    try {
      const input = {
        id: scene.id,
        name: scene.name,
        description: scene.description || '',
        dramatis_personae: scene.dramatis_personae || []
      }
      
      const response = await API.graphql(graphqlOperation(updateScene, { input }))
      return response.data.updateScene
    } catch (error) {
      throw error
    }
  },

  async deleteScene(id) {
    if (!id || id.trim() === '') {
      throw new Error('Scene ID is required')
    }

    try {
      const input = { id }
      const response = await API.graphql(graphqlOperation(deleteScene, { input }))
      return response.data.deleteScene
    } catch (error) {
      throw error
    }
  },

  async getSceneByName(name) {
    if (!name || name.trim() === '') {
      throw new Error('Scene name is required')
    }

    try {
      const scenes = await this.listScenes()
      const scene = scenes.find(scene => scene.name === name)
      return scene || null
    } catch (error) {
      throw error
    }
  }
}

export default sceneService