import { PageHeader } from 'bootstrap-react-components'
import React, { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { useMutation, useQuery } from 'react-query'
import SceneEditor from '../components/scene/SceneEditor'
import sceneService from '../services/sceneService'
// import { gameEntityService } from '../services'
import Scene from '../models/Scene'

export default function SceneAddPage() {
  const navigate = useNavigate()
  const [scene, setScene] = useState(new Scene())

  // Fetch available characters (assuming they exist in the system)
  const { data: availableCharacters = [] } = useQuery(
    ['characters'],
    async () => {
      try {
        // This would normally fetch from a character service
        // For now, return some mock data
        return [
          { name: 'Sir Gareth', description: 'A noble knight' },
          { name: 'Mara', description: 'A cunning thief' },
          { name: 'Grimjaw', description: 'An orc bartender' },
          { name: 'Eldara', description: 'An elven mage' }
        ]
      } catch (error) {
        console.error('Failed to fetch characters:', error)
        return []
      }
    }
  )

  // Create scene mutation
  const createSceneMutation = useMutation(
    (newScene) => sceneService.createScene(newScene),
    {
      onSuccess: () => {
        navigate('/scenes')
      },
      onError: (error) => {
        console.error('Failed to create scene:', error)
        alert('Failed to create scene. Please try again.')
      }
    }
  )

  const handleSceneChange = (updatedScene) => {
    setScene(updatedScene)
  }

  const handleSaveScene = async (sceneToSave) => {
    try {
      await createSceneMutation.mutateAsync(sceneToSave)
    } catch (error) {
      // Error handling is done in the mutation
    }
  }

  const handleCancel = () => {
    navigate('/scenes')
  }

  return (
    <div id='SceneAddPage'>
      <PageHeader id='SceneAddPage'>
        <h1>Add New Scene</h1>
      </PageHeader>

      <div className="row">
        <div className="col-lg-8">
          <div className="card">
            <div className="card-header">
              <h3 className="card-title mb-0">Scene Details</h3>
            </div>
            <div className="card-body" id="scene-edit-form">
              <SceneEditor
                scene={scene}
                availableCharacters={availableCharacters}
                onChange={handleSceneChange}
                onSave={handleSaveScene}
              />
            </div>
          </div>
        </div>
        
        <div className="col-lg-4">
          <div className="card">
            <div className="card-header">
              <h4 className="card-title mb-0">Actions</h4>
            </div>
            <div className="card-body">
              <div className="d-grid gap-2">
                <button
                  type="button"
                  className="btn btn-success"
                  onClick={() => handleSaveScene(scene)}
                  disabled={createSceneMutation.isLoading}
                  id="button-save-Scene"
                >
                  {createSceneMutation.isLoading ? 'Saving...' : 'Save Scene'}
                </button>
                
                <button
                  type="button"
                  className="btn btn-secondary"
                  onClick={handleCancel}
                  disabled={createSceneMutation.isLoading}
                >
                  Cancel
                </button>
              </div>

              {createSceneMutation.isError && (
                <div className="alert alert-danger mt-3">
                  <small>
                    Failed to save scene. Please check your inputs and try again.
                  </small>
                </div>
              )}
            </div>
          </div>

          <div className="card mt-3">
            <div className="card-header">
              <h4 className="card-title mb-0">Tips</h4>
            </div>
            <div className="card-body">
              <ul className="list-unstyled small text-muted">
                <li>• Give your scene a descriptive name</li>
                <li>• Add characters who will appear in this scene</li>
                <li>• Describe each character's role or importance</li>
                <li>• You can edit these details later</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}