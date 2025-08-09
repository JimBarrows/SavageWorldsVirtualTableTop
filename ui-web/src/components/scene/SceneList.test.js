import React from 'react'
import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import SceneList from './SceneList'

const renderSceneList = (props) => {
  return render(
    <MemoryRouter>
      <SceneList {...props} />
    </MemoryRouter>
  )
}

describe('SceneList', () => {
  const mockScenes = [
    {
      id: '1',
      name: 'Opening Tavern Scene',
      description: 'The heroes gather at the Red Dragon Inn',
      dramatis_personae: [
        { name: 'Sir Gareth', description: 'A noble knight' },
        { name: 'Mara', description: 'A cunning thief' }
      ]
    },
    {
      id: '2',
      name: 'Forest Ambush',
      description: 'Bandits attack on the forest road',
      dramatis_personae: [
        { name: 'Bandit Leader', description: 'Ruthless highway robber' }
      ]
    }
  ]

  describe('rendering', () => {
    test('should render empty table when no scenes provided', () => {
      renderSceneList({ id: 'scene-list', scenes: [] })
      
      expect(screen.getByRole('table')).toBeInTheDocument()
      expect(screen.getByText('Name')).toBeInTheDocument()
      expect(screen.getByText('Description')).toBeInTheDocument()
      expect(screen.getByText('Characters')).toBeInTheDocument()
    })

    test('should render scene list with data', () => {
      renderSceneList({ id: 'scene-list', scenes: mockScenes })
      
      expect(screen.getByText('Opening Tavern Scene')).toBeInTheDocument()
      expect(screen.getByText('The heroes gather at the Red Dragon Inn')).toBeInTheDocument()
      expect(screen.getByText('Forest Ambush')).toBeInTheDocument()
      expect(screen.getByText('Bandits attack on the forest road')).toBeInTheDocument()
    })

    test('should display character count for each scene', () => {
      renderSceneList({ id: 'scene-list', scenes: mockScenes })
      
      expect(screen.getByText('2 characters')).toBeInTheDocument()
      expect(screen.getByText('1 character')).toBeInTheDocument()
    })

    test('should render edit links for each scene', () => {
      renderSceneList({ id: 'scene-list', scenes: mockScenes })
      
      const editLinks = screen.getAllByText('Edit')
      expect(editLinks).toHaveLength(2)
    })

    test('should render delete buttons for each scene', () => {
      renderSceneList({ id: 'scene-list', scenes: mockScenes })
      
      const deleteButtons = screen.getAllByText('Delete')
      expect(deleteButtons).toHaveLength(2)
    })
  })

  describe('props', () => {
    test('should handle missing scenes prop gracefully', () => {
      renderSceneList({ id: 'scene-list' })
      
      expect(screen.getByRole('table')).toBeInTheDocument()
    })

    test('should apply provided id', () => {
      renderSceneList({ id: 'custom-scene-list', scenes: [] })
      
      expect(screen.getByTestId('custom-scene-list')).toBeInTheDocument()
    })
  })

  describe('callbacks', () => {
    test('should call onDelete when delete button clicked', () => {
      const onDelete = jest.fn()
      renderSceneList({ 
        id: 'scene-list', 
        scenes: [mockScenes[0]], 
        onDelete 
      })
      
      const deleteButton = screen.getByText('Delete')
      deleteButton.click()
      
      expect(onDelete).toHaveBeenCalledWith('1')
    })

    test('should not fail when onDelete is not provided', () => {
      renderSceneList({ id: 'scene-list', scenes: [mockScenes[0]] })
      
      const deleteButton = screen.getByText('Delete')
      expect(() => deleteButton.click()).not.toThrow()
    })
  })
})