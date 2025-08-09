import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { Button, PageHeader } from 'bootstrap-react-components'
import React from 'react'
import { useNavigate } from 'react-router-dom'
import { useQuery, useMutation, useQueryClient } from 'react-query'
import SceneList from '../components/scene/SceneList'
import sceneService from '../services/sceneService'

export default function SceneListPage() {
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  
  // Fetch scenes using React Query
  const { data, isLoading, isError, error } = useQuery(
    ['scenes'],
    () => sceneService.listScenes()
  )

  // Delete scene mutation
  const deleteSceneMutation = useMutation(
    (sceneId) => sceneService.deleteScene(sceneId),
    {
      onSuccess: () => {
        // Invalidate and refetch scenes list
        queryClient.invalidateQueries(['scenes'])
      }
    }
  )

  const navigateToNewScene = () => navigate('/scene/add')

  const handleDeleteScene = async (sceneId) => {
    if (window.confirm('Are you sure you want to delete this scene?')) {
      try {
        await deleteSceneMutation.mutateAsync(sceneId)
      } catch (error) {
        console.error('Failed to delete scene:', error)
        alert('Failed to delete scene. Please try again.')
      }
    }
  }

  const sceneList = () => (
    <SceneList 
      id={'mainSceneList'} 
      scenes={data || []} 
      onDelete={handleDeleteScene}
    />
  )

  if (isLoading) {
    return (
      <div id='SceneListPage'>
        <PageHeader id='SceneListPage'><h1>Scenes</h1></PageHeader>
        <div className="text-center mt-5">
          <div className="spinner-border" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          <p className="mt-2">Loading scenes...</p>
        </div>
      </div>
    )
  }

  if (isError) {
    return (
      <div id='SceneListPage'>
        <PageHeader id='SceneListPage'><h1>Scenes</h1></PageHeader>
        <div className="alert alert-danger mt-3" role="alert">
          <h4 className="alert-heading">Error loading scenes</h4>
          <p>{error?.message || 'An unexpected error occurred'}</p>
          <hr />
          <p className="mb-0">
            Please try refreshing the page or contact support if the problem persists.
          </p>
        </div>
      </div>
    )
  }

  return (
    <div id='SceneListPage'>
      <PageHeader id='SceneListPage'><h1>Scenes</h1></PageHeader>
      <div className="d-flex justify-content-between align-items-center mb-3">
        <div>
          {data && data.length > 0 && (
            <p className="text-muted mb-0">{data.length} scene{data.length !== 1 ? 's' : ''} found</p>
          )}
        </div>
        <Button 
          id='button-addScene'
          className="btn btn-primary"
          onClick={navigateToNewScene}
        >
          <FontAwesomeIcon icon="plus" className="me-2" />
          Add Scene
        </Button>
      </div>
      
      {data && data.length > 0 ? (
        sceneList()
      ) : (
        <div className="text-center mt-5">
          <FontAwesomeIcon icon="theater-masks" size="4x" className="text-muted mb-3" />
          <h3>No Scenes Yet</h3>
          <p className="text-muted">Get started by creating your first scene.</p>
          <Button 
            className="btn btn-primary"
            onClick={navigateToNewScene}
          >
            <FontAwesomeIcon icon="plus" className="me-2" />
            Create First Scene
          </Button>
        </div>
      )}
    </div>
  )
}