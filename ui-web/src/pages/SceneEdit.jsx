import { PageHeader } from 'bootstrap-react-components'
import React, { useState } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import { useMutation, useQuery } from 'react-query'
import SceneEditor from '../components/scene/SceneEditor'
import sceneService from '../services/sceneService'
import Scene from '../models/Scene'

export default function SceneEditPage() {
  const navigate = useNavigate()
  const { name } = useParams()
  const [scene, setScene] = useState(new Scene())

  // Fetch the scene to edit
  const { data: sceneData, isLoading, isError } = useQuery(
    ['scene', name],
    () => sceneService.getSceneByName(decodeURIComponent(name)),
    {
      enabled: !!name,
      onSuccess: (data) => {
        if (data) {
          setScene(data)
        }
      }
    }
  )

  // Fetch available characters
  const { data: availableCharacters = [] } = useQuery(
    ['characters'],
    async () => {
      // Mock data for now
      return [
        { name: 'Sir Gareth', description: 'A noble knight' },
        { name: 'Mara', description: 'A cunning thief' },
        { name: 'Grimjaw', description: 'An orc bartender' },
        { name: 'Eldara', description: 'An elven mage' }
      ]
    }
  )

  // Update scene mutation
  const updateSceneMutation = useMutation(
    (updatedScene) => sceneService.updateScene(updatedScene),
    {
      onSuccess: () => {
        navigate('/scenes')
      },
      onError: (error) => {
        console.error('Failed to update scene:', error)
        alert('Failed to update scene. Please try again.')
      }
    }
  )

  const handleSceneChange = (updatedScene) => {
    setScene(updatedScene)
  }

  const handleSaveScene = async (sceneToSave) => {
    try {
      await updateSceneMutation.mutateAsync(sceneToSave)
    } catch (error) {
      // Error handling is done in the mutation
    }
  }

  const handleCancel = () => {
    navigate('/scenes')
  }

  if (isLoading) {
    return (
      <div id='SceneEditPage'>
        <PageHeader id='SceneEditPage'>
          <h1>Edit Scene</h1>
        </PageHeader>
        <div className="text-center mt-5">
          <div className="spinner-border" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          <p className="mt-2">Loading scene...</p>
        </div>
      </div>
    )
  }

  if (isError || !sceneData) {
    return (
      <div id='SceneEditPage'>
        <PageHeader id='SceneEditPage'>
          <h1>Edit Scene</h1>
        </PageHeader>
        <div className="alert alert-danger mt-3" role="alert">
          <h4 className="alert-heading">Scene not found</h4>
          <p>The scene "{name}" could not be found.</p>
          <hr />
          <button 
            className="btn btn-primary" 
            onClick={() => navigate('/scenes')}
          >
            Back to Scenes
          </button>
        </div>
      </div>
    )
  }

  return (
    <div id='SceneEditPage'>
      <PageHeader id='SceneEditPage'>
        <h1>Edit Scene: {scene.name}</h1>
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
                  disabled={updateSceneMutation.isLoading}
                  id="button-save-Scene"
                >
                  {updateSceneMutation.isLoading ? 'Saving...' : 'Save Changes'}
                </button>
                
                <button
                  type="button"
                  className="btn btn-secondary"
                  onClick={handleCancel}
                  disabled={updateSceneMutation.isLoading}
                >
                  Cancel
                </button>
              </div>

              {updateSceneMutation.isError && (
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
              <h4 className="card-title mb-0">Scene Info</h4>
            </div>
            <div className="card-body">
              <ul className="list-unstyled small text-muted">
                <li><strong>ID:</strong> {scene.id}</li>
                <li><strong>Characters:</strong> {scene.dramatis_personae?.length || 0}</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}