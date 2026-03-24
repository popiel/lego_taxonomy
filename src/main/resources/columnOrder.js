(function (root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory();
  } else {
    root.columnOrder = factory();
  }
}(typeof self !== 'undefined' ? self : this, function () {
  const COLUMN_IDS = ['category', 'category2', 'category3', 'category4', 'image', 'color', 'quantity', 'name', 'partNumber'];
  const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];

  function getCategoryRange(order) {
    const indices = order
      .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
      .filter(idx => idx >= 0);
    if (indices.length === 0) return { start: -1, end: -1 };
    return { start: indices[0], end: indices[indices.length - 1] };
  }

  function getInsertIndex(order, draggedColumnIndex, isDraggingCategory, dropPosition) {
    const n = order.length;
    const range = getCategoryRange(order);

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    if (draggedIndices.length === 0) return -1;

    const minDragged = Math.min(...draggedIndices);
    const maxDragged = Math.max(...draggedIndices);

    // Check if this is a no-op (dropping at current position)
    if (dropPosition === minDragged || dropPosition === maxDragged + 1) {
      return -1;
    }

    let insertIndex;

    if (isDraggingCategory) {
      // For category moves:
      // - If dropping at position 0, allow it
      // - If dropping within category range (where categories currently are), clamp to after range
      // - If dropping after category range, allow it
      let effectivePos;
      if (dropPosition === 0) {
        effectivePos = 0;
      } else if (dropPosition >= range.start && dropPosition <= range.end) {
        // Dropping within current category range - clamp to after range
        effectivePos = range.end + 1;
      } else {
        effectivePos = dropPosition;
      }
      
      if (effectivePos === 0) {
        insertIndex = 0;
      } else if (effectivePos > maxDragged) {
        insertIndex = effectivePos - draggedIndices.length;
      } else {
        insertIndex = effectivePos;
      }
    } else {
      // For non-category moves
      let effectiveDropPosition = dropPosition;
      
      // If dropping within category range, move to end of category range
      if (dropPosition > range.start && dropPosition <= range.end) {
        effectiveDropPosition = range.end + 1;
      }
      
      if (effectiveDropPosition > maxDragged) {
        insertIndex = effectiveDropPosition - draggedIndices.length;
      } else {
        insertIndex = effectiveDropPosition;
      }
    }

    return insertIndex;
  }

  function moveToDropPosition(order, draggedColumnIndex, isDraggingCategory, dropPosition) {
    const n = order.length;

    let draggedIndices;
    if (isDraggingCategory) {
      draggedIndices = [];
      for (let i = 0; i < n; i++) {
        if (CATEGORY_COLUMNS.includes(order[i])) {
          draggedIndices.push(i);
        }
      }
    } else {
      draggedIndices = [draggedColumnIndex];
    }

    if (draggedIndices.length === 0) return [...order];

    const insertIndex = getInsertIndex(order, draggedColumnIndex, isDraggingCategory, dropPosition);

    if (insertIndex === -1) {
      return [...order];
    }

    // Sort indices in descending order for removal
    const sortedIndices = [...draggedIndices].sort((a, b) => b - a);

    let newOrder = [...order];
    for (const idx of sortedIndices) {
      newOrder.splice(idx, 1);
    }

    // Insert at new position
    if (isDraggingCategory) {
      const columnsToInsert = draggedIndices.map(idx => order[idx]);
      newOrder.splice(insertIndex, 0, ...columnsToInsert);
    } else {
      newOrder.splice(insertIndex, 0, order[draggedColumnIndex]);
    }

    return newOrder;
  }

  function resetColumns() {
    return [...COLUMN_IDS];
  }

  return {
    COLUMN_IDS,
    CATEGORY_COLUMNS,
    getCategoryRange,
    moveToDropPosition,
    resetColumns
  };
}));
