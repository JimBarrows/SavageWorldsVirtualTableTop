import { PageHeader } from 'bootstrap-react-components';
import * as PropTypes from 'prop-types';
import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useMutation, useQueryClient } from 'react-query';
import PlotPointForm from '../components/plotpoint/editor';
import { plotPointService } from '../services';
import PlotPoint from '../models/PlotPoint';

export default function PlotPointAdd({ id = 'PlotPointEditorPage' }) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [errors, setErrors] = useState([]);

  // Create mutation for adding plot point
  const createMutation = useMutation(
    (plotPoint) => plotPointService.createPlotPoint(plotPoint),
    {
      onSuccess: () => {
        // Invalidate and refetch plot points list
        queryClient.invalidateQueries(['plotPoints']);
      navigate('/');
    },
    onError: (error) => {
      const errorMessages = error.errors || [error.message || 'Failed to create plot point'];
      setErrors(Array.isArray(errorMessages) ? errorMessages : [errorMessages]);
    }
  });

  const cancel = () => {
    navigate('/');
  };

  const save = async (plotPoint) => {
    // Remove id if it exists (for new plot points)
    const { id, ...plotPointData } = plotPoint;
    
    // Clear previous errors
    setErrors([]);
    
    // Execute mutation
    createMutation.mutate(plotPointData);
  };

  return (
    <div id={id}>
      <PageHeader id={id}>
        <h1>New Plot Point</h1>
      </PageHeader>
      
      {createMutation.isLoading && (
        <div className="alert alert-info" role="alert">
          <div className="spinner-border spinner-border-sm me-2" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          Creating plot point...
        </div>
      )}
      
      <PlotPointForm 
        id={'plotPointAdd'}
        errors={errors}
        onSave={save}
        onCancel={cancel}
        plotPoint={new PlotPoint()}
        disabled={createMutation.isLoading}
      />
    </div>
  );
}

PlotPointAdd.propTypes = {
  id: PropTypes.string
};