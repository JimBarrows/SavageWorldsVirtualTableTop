package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.PlotPoint;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface PlotPointRepository extends PagingAndSortingRepository<PlotPoint, Long> {

	List<PlotPoint> findByName(String name);

}
