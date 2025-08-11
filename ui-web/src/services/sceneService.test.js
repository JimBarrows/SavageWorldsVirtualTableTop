// TODO: Enable these tests once AWS Amplify is properly configured
// import sceneService from './sceneService'
// import Scene from '../models/Scene'

// Mock AWS Amplify
// const mockAPI = {
//   graphql: jest.fn()
// }
// const mockGraphqlOperation = jest.fn(query => ({ query }))

// jest.mock('aws-amplify', () => ({
//   API: mockAPI,
//   graphqlOperation: mockGraphqlOperation
// }))

/*
describe.skip('sceneService', () => {
  const mockScene = {
    id: '1',
    name: 'Opening Tavern Scene',
    description: 'The heroes gather at the Red Dragon Inn',
    dramatis_personae: [
      { name: 'Sir Gareth', description: 'A noble knight' },
      { name: 'Mara', description: 'A cunning thief' }
    ]
  }

  beforeEach(() => {
    jest.clearAllMocks()
  })

  describe('listScenes', () => {
    test('should fetch all scenes from API', async () => {
      const mockResponse = {
        data: {
          listScenes: {
            items: [mockScene]
          }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.listScenes()

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result).toEqual([mockScene])
    })

    test('should handle API errors gracefully', async () => {
      const mockError = new Error('API Error')
      mockAPI.graphql.mockRejectedValue(mockError)

      await expect(sceneService.listScenes()).rejects.toThrow('API Error')
    })

    test('should return empty array when no items', async () => {
      const mockResponse = {
        data: {
          listScenes: {
            items: []
          }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.listScenes()

      expect(result).toEqual([])
    })
  })

  describe('getScene', () => {
    test('should fetch scene by id', async () => {
      const mockResponse = {
        data: {
          getScene: mockScene
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.getScene('1')

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result).toEqual(mockScene)
    })

    test('should throw error for missing id', async () => {
      await expect(sceneService.getScene()).rejects.toThrow('Scene ID is required')
      await expect(sceneService.getScene('')).rejects.toThrow('Scene ID is required')
      await expect(sceneService.getScene(null)).rejects.toThrow('Scene ID is required')
    })

    test('should handle scene not found', async () => {
      const mockResponse = {
        data: {
          getScene: null
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.getScene('nonexistent')

      expect(result).toBeNull()
    })
  })

  describe('createScene', () => {
    test('should create new scene', async () => {
      const newScene = new Scene()
      newScene.name = 'New Scene'
      newScene.description = 'A new scene'

      const mockResponse = {
        data: {
          createScene: { ...newScene, id: '123' }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.createScene(newScene)

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result.id).toBe('123')
      expect(result.name).toBe('New Scene')
    })

    test('should validate scene before creation', async () => {
      await expect(sceneService.createScene()).rejects.toThrow('Scene is required')
      await expect(sceneService.createScene(null)).rejects.toThrow('Scene is required')
      await expect(sceneService.createScene({})).rejects.toThrow('Scene name is required')
      await expect(sceneService.createScene({ name: '' })).rejects.toThrow('Scene name is required')
    })

    test('should handle creation errors', async () => {
      const mockError = new Error('Creation failed')
      mockAPI.graphql.mockRejectedValue(mockError)

      const newScene = new Scene()
      newScene.name = 'New Scene'

      await expect(sceneService.createScene(newScene)).rejects.toThrow('Creation failed')
    })
  })

  describe('updateScene', () => {
    test('should update existing scene', async () => {
      const updatedScene = { ...mockScene, description: 'Updated description' }
      const mockResponse = {
        data: {
          updateScene: updatedScene
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.updateScene(updatedScene)

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result.description).toBe('Updated description')
    })

    test('should validate scene before update', async () => {
      await expect(sceneService.updateScene()).rejects.toThrow('Scene is required')
      await expect(sceneService.updateScene({})).rejects.toThrow('Scene ID is required')
      await expect(sceneService.updateScene({ id: '' })).rejects.toThrow('Scene ID is required')
      await expect(sceneService.updateScene({ id: '1' })).rejects.toThrow('Scene name is required')
    })

    test('should handle update errors', async () => {
      const mockError = new Error('Update failed')
      mockAPI.graphql.mockRejectedValue(mockError)

      await expect(sceneService.updateScene(mockScene)).rejects.toThrow('Update failed')
    })
  })

  describe('deleteScene', () => {
    test('should delete scene by id', async () => {
      const mockResponse = {
        data: {
          deleteScene: { id: '1' }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.deleteScene('1')

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result.id).toBe('1')
    })

    test('should validate id before deletion', async () => {
      await expect(sceneService.deleteScene()).rejects.toThrow('Scene ID is required')
      await expect(sceneService.deleteScene('')).rejects.toThrow('Scene ID is required')
      await expect(sceneService.deleteScene(null)).rejects.toThrow('Scene ID is required')
    })

    test('should handle deletion errors', async () => {
      const mockError = new Error('Deletion failed')
      mockAPI.graphql.mockRejectedValue(mockError)

      await expect(sceneService.deleteScene('1')).rejects.toThrow('Deletion failed')
    })
  })

  describe('getSceneByName', () => {
    test('should fetch scene by name', async () => {
      const mockResponse = {
        data: {
          listScenes: {
            items: [mockScene]
          }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.getSceneByName('Opening Tavern Scene')

      expect(mockAPI.graphql).toHaveBeenCalledWith(
        expect.objectContaining({ query: expect.any(String) })
      )
      expect(result).toEqual(mockScene)
    })

    test('should return null when scene not found by name', async () => {
      const mockResponse = {
        data: {
          listScenes: {
            items: []
          }
        }
      }
      mockAPI.graphql.mockResolvedValue(mockResponse)

      const result = await sceneService.getSceneByName('Nonexistent Scene')

      expect(result).toBeNull()
    })

    test('should validate name parameter', async () => {
      await expect(sceneService.getSceneByName()).rejects.toThrow('Scene name is required')
      await expect(sceneService.getSceneByName('')).rejects.toThrow('Scene name is required')
      await expect(sceneService.getSceneByName(null)).rejects.toThrow('Scene name is required')
    })
  })
})
*/